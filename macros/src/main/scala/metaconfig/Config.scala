package metaconfig

import scala.annotation.compileTimeOnly
import scala.collection.immutable.Seq
import scala.meta._
import scala.meta.tokens.Token.Constant
import scala.util.control.NonFatal

class Error(msg: String) extends Exception(msg)
case class ConfigError(msg: String) extends Error(msg)
case class ConfigErrors(es: Seq[Throwable])
    extends Error(s"Errors: ${es.mkString("\n")}")

trait Reader[T] {
  def read(any: Any): Result[T]
}

object String2AnyMap {
  def unapply(arg: Any): Option[Map[String, Any]] = arg match {
    case someMap: Map[_, _] =>
      try {
        Some(someMap.asInstanceOf[Map[String, Any]])
      } catch {
        case _: ClassCastException =>
          None
      }
  }
}
object Reader {
  def instance[T](f: PartialFunction[Any, Result[T]]) =
    new Reader[T] {
      override def read(any: Any): Result[T] =
        try {
          f.applyOrElse(
            any,
            (x: Any) =>
              Left(
                new IllegalArgumentException(
                  s"value '$x' of type ${x.getClass.getSimpleName}.")))
        } catch {
          case e: ConfigError => Left(e)
        }
    }
  implicit val intR = instance[Int] { case x: Int => Right(x) }
  implicit val stringR = instance[String] { case x: String => Right(x) }
  implicit val boolR = instance[scala.Boolean] {
    case x: Boolean => Right(x)
  }
  def seqR[T: Reader]: Reader[Seq[T]] = instance[Seq[T]] {
    case lst: Seq[_] =>
      val res = lst.map { t =>
        implicitly[Reader[T]].read(t)
      }
      val lefts = res.collect { case Left(e) => e }
      if (lefts.nonEmpty) Left(ConfigErrors(lefts))
      else Right(res.collect { case Right(e) => e })
  }

  implicit val intsR: Reader[Seq[Int]] = seqR[Int]
}

@compileTimeOnly("@metaconfig.Config not expanded")
class Config extends scala.annotation.StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    def genReader(typ: Type, params: Seq[Term.Param]): Defn.Val = {
      def defaultArgs: Seq[Term.Arg] = {
        params.collect {
          case p if p.name.is[Term.Name] && p.decltpe.exists(_.is[Type]) =>
            val nameLit = Lit(p.name.syntax)
            val pTyp = p.decltpe.get.asInstanceOf[Type]
            val pName = p.name.asInstanceOf[Term.Name]
            Term.Arg.Named(p.name.asInstanceOf[Term.Name],
                           q"get[$pTyp]($nameLit, $pName)")
        }
      }
      val argLits = params.map(x => Lit(x.name.syntax))
      NonFatal
      val classLit = Lit(typ.syntax)
      val constructor = Ctor.Ref.Name(typ.syntax)
      q"""val reader = new _root_.metaconfig.Reader[$typ] {
          override def read(any: Any): _root_.metaconfig.Result[$typ] = {
            any match {
              case _root_.metaconfig.String2AnyMap(map) =>
                def get[T : Reader](
                    path: String, default: T) = {
                    val ev =implicitly[Reader[T]]

                  ev.read(map.getOrElse(path, default)) match {
                    case Right(e) => e.asInstanceOf[T]
                    case Left(e: java.lang.IllegalArgumentException) =>
                      val msg =
                        "Error reading field " + path +
                        " on class " + $classLit +
                        ". Expected argument of type " + default.getClass.getSimpleName +
                        ". Obtained " + e.getMessage()
                      throw _root_.metaconfig.ConfigError(msg)
                    case Left(e) => throw e
                  }
                }
                val validFields = _root_.scala.collection.immutable.Set(..$argLits)
                val invalidFields = map.keys.filterNot(validFields)
                if (invalidFields.nonEmpty) {
                  val msg = "Invalid fields: " + invalidFields.mkString(", ")
                  Left(new _root_.metaconfig.ConfigError(msg))
                } else {
                  try {
                    import _root_.metaconfig.Reader._
                    Right(new $constructor(..$defaultArgs))
                  } catch {
                    case _root_.scala.util.control.NonFatal(e) => Left(e)
                  }
                }
            }
          }
        }
     """
    }
    defn match {
      case q"..$mods class $tname[..$tparams] ..$mods2 (...$paramss) extends $template" =>
        val template"{ ..$earlyStats } with ..$ctorcalls { $param => ..$stats }" =
          template
        println(stats, paramss.flatten)
        val flatParams = paramss.flatten
        val types: Seq[Type.Arg] = flatParams.collect {
          case x if x.decltpe.isDefined =>
            x.decltpe.get
        }
        val fields: Seq[Term.Tuple] = flatParams.collect {
          case x if x.name.isInstanceOf[Term.Name] =>
            val nameLit = Lit(x.name.syntax)
            q"($nameLit, ${x.name.asInstanceOf[Term.Name]})"
        }

        val fieldsDef: Stat = {
          val body =
            Term.Apply(q"_root_.scala.collection.immutable.Map", fields)
          q"def fields = $body"
        }
        val typReader = genReader(tname, flatParams)
        val newStats = Seq(fieldsDef) ++ Seq(typReader) ++ stats
        println(types)
        val newTemplate = template"""
        { ..$earlyStats } with ..$ctorcalls { $param => ..$newStats }

                                  """
        val result =
          q"""
            ..$mods class $tname[..$tparams] ..$mods2 (...$paramss) extends $newTemplate
         """
        println(result.syntax)
        result
      case els =>
        ???
    }
  }
}
