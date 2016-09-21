package metaconfig

import scala.annotation.compileTimeOnly
import scala.collection.immutable.Seq
import scala.meta._
import scala.meta.tokens.Token.Constant

trait Reader[T] {
  def read(any: Any): Result[T]
}
object Reader {
  def instance[T](f: PartialFunction[Any, Result[T]]) =
    new Reader[T] {
      override def read(any: Any) = f(any)
    }
  implicit val intR = instance[Int] { case x: Int => Right(x) }
  implicit val stringR = instance[String] { case x: String => Right(x) }
  implicit val boolR = instance[scala.Boolean] {
    case x: Boolean => Right(x)
  }
}

@compileTimeOnly("@metaconfig.Config not expanded")
class Config extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
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
        val implicitStats = types.collect {
          case typ: Type =>
            q"""_root_.scala.Predef.implicitly[_root_.io.circe.Decoder[$typ]]"""
        }

        val fieldsDef: Seq[Stat] = {
          val body =
            Term.Apply(q"_root_.scala.collection.immutable.Map", fields)
          Seq(q"def fields = $body")
        }
        val newStats = implicitStats ++ fieldsDef ++ stats
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
