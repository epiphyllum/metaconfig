package metaconfig

import scala.annotation.compileTimeOnly
import scala.meta._

@compileTimeOnly("@metaconfig.Config not expanded")
class Config extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case q"..$mods class $tname[..$tparams] ..$mods2 (...$paramss) extends $template" =>
        val template"{ ..$earlyStats } with ..$ctorcalls { $param => ..$stats }" = template
        println(stats, paramss.flatten)
        q"""
            ..$mods class $tname[..$tparams] ..$mods2 (...$paramss) extends $template
         """
        defn
      case els =>
        ???
    }
  }
}

