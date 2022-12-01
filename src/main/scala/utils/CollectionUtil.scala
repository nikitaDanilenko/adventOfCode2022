package utils

object CollectionUtil {

  def splitOn[A](a: A, as: Seq[A]): Seq[Seq[A]] = {
    if (as.isEmpty)
      Seq.empty
    else {
      val (init, tail) = as.span(_ != a)
      init +: splitOn(a, tail.drop(1))
    }
  }

  object syntax {

    implicit class SplitOn[A](as: Seq[A]) extends AnyVal {
      def splitOn(a: A): Seq[Seq[A]] = CollectionUtil.splitOn(a, as)
    }

  }

}
