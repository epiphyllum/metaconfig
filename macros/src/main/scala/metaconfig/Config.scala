package metaconfig

import scala.annotation.compileTimeOnly
import scala.collection.immutable.Seq
import scala.meta._
import scala.meta.tokens.Token.Constant

trait IsConfig[T] {
  def fields: Seq[String] = Nil
}
object IsConfig{
  implicit object IntD extends IsConfig[Int]
  implicit object StringD extends IsConfig[String]
  implicit object BoolD extends IsConfig[Boolean]
}

@compileTimeOnly("@metaconfig.Config not expanded")
class Config extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case q"..$mods class $tname[..$tparams] ..$mods2 (...$paramss) extends $template" =>
        val template"{ ..$earlyStats } with ..$ctorcalls { $param => ..$stats }" = template
        println(stats, paramss.flatten)
        val flatParams = paramss.flatten
        val types: Seq[Type.Arg] = flatParams.collect {
          case x if x.decltpe.isDefined =>
            x.decltpe.get
        }
        val fields: Seq[Lit] = flatParams.map{ x =>
          Lit(x.name.syntax)
        }
        val implicitStats = types.map {
          typ =>
            q"_root_.scala.Predef.implicitly[_root_.metaconfig.IsConfig[${typ.asInstanceOf[Type]}]]"
        }

        val fieldsDef: Seq[Stat] = {
          val body = Term.Apply(q"_root_.scala.collection.immutable.Seq", fields)
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

