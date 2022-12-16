package utils

object CollectionUtil {

  def splitOn[A](a: A, as: Seq[A]): Seq[Seq[A]] = {
    if (as.isEmpty) Seq.empty
    else {
      val (init, tail) = as.span(_ != a)
      init +: splitOn(a, tail.drop(1))
    }
  }

  def unionWith[K, V](map1: Map[K, V], map2: Map[K, V])(f: (V, V) => V): Map[K, V] = {
    val allKeys = map1.keySet ++ map2.keySet
    allKeys.flatMap { key =>
      val value = (map1.get(key), map2.get(key)) match {
        case (Some(v1), Some(v2)) => Some(f(v1, v2))
        case (x @ Some(_), _)     => x
        case (_, y @ Some(_))     => y
        case _                    => None
      }
      value.map(key -> _)
    }.toMap
  }

  object syntax {

    implicit class SplitOn[A](as: Seq[A]) extends AnyVal {
      def splitOn(a: A): Seq[Seq[A]] = CollectionUtil.splitOn(a, as)
    }

  }

}
