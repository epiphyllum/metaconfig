package metaconfig

import scala.annotation.compileTimeOnly
import scala.collection.immutable.Seq
import scala.meta._
import scala.meta.tokens.Token.Constant

class Error(msg: String) extends Exception(msg)
case class ConfigError(msg: String) extends Error(msg)
case class ConfigErrors(es: Seq[Throwable])
    extends Error(s"Errors: ${es.mkString("\n")}")

@compileTimeOnly("@metaconfig.Config not expanded")
class Config extends scala.annotation.StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    def genReader(typ: Type, params: Seq[Term.Param]): Defn.Val = {
      def defaultArgs: Seq[Term.Arg] = {
        params.collect {
          case Term.Param(_, pName: Term.Name, Some(pTyp: Type), _) =>
            val nameLit = Lit(pName.syntax)
            Term.Arg.Named(pName, q"get[$pTyp]($nameLit, $pName)")
        }
      }
      val argLits = params.map(x => Lit(x.name.syntax))
      val classLit = Lit(typ.syntax)
      val constructor = Ctor.Ref.Name(typ.syntax)
      q"""val reader = new _root_.metaconfig.Reader[$typ] {
          override def read(any: Any): _root_.metaconfig.Result[$typ] = {
            any match {
              case _root_.metaconfig.String2AnyMap(map) =>
                def get[T : Reader](path: String, default: T) = {
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
                  Left(_root_.metaconfig.ConfigError(msg))
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
    Defn.Def
    defn match {
      case q"..$mods class $tname[..$tparams] ..$mods2 (...$paramss) extends $template" =>
        val template"{ ..$earlyStats } with ..$ctorcalls { $param => ..$stats }" =
          template
        val flatParams = paramss.flatten
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
        val newTemplate = template"""
        { ..$earlyStats } with ..$ctorcalls { $param => ..$newStats }

                                  """
        val result =
          q"""
            ..$mods class $tname[..$tparams] ..$mods2 (...$paramss) extends $newTemplate
         """
        println(result)
        result
      case els => abort("Failed to expand...")
    }
  }
}
