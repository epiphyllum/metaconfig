package metaconfig

import scala.annotation.compileTimeOnly
import scala.collection.immutable
import scala.meta._

trait Decoder[T]
object Decoder{
  implicit object IntD extends Decoder[Int]
  implicit object StringD extends Decoder[String]
  implicit object BoolD extends Decoder[Boolean]
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
        val implicitStats = types.map {
          typ =>
            q"implicitly[_root_.metaconfig.Decoder[${typ.asInstanceOf[Type]}]]"
        }
        val newStats = immutable.Seq(implicitStats ++ stats:_*)
        println(types)
        val newTemplate = template"""
        { ..$earlyStats } with ..$ctorcalls { $param => ..${newStats} }

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

