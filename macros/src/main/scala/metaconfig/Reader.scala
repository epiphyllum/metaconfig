package metaconfig

import scala.collection.immutable.Seq

trait Reader[+T] {
  def read(any: Any): Result[T]
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
  implicit def seqR[T](implicit ev: Reader[T]): Reader[Seq[T]] =
    instance[Seq[T]] {
      case lst: Seq[_] =>
        val res = lst.map(ev.read)
        val lefts = res.collect { case Left(e) => e }
        if (lefts.nonEmpty) Left(ConfigErrors(lefts))
        else Right(res.collect { case Right(e) => e })
    }
}
