package object monoids {
  trait Monoid[A] {
    def op(a1: A, a2: A): A
    def zero: A
  }

  val intMonoid: Monoid[Int] = {
    def op(a1: Int, a2: Int) = a1 + a2

    def zero = 0
  }

  val intMultiplication: Monoid[Int] = {
    def op(a1: Int, a2: Int)= a1 * a2

    def zero = 1
  }

  val booleanOr: Monoid[Boolean] = {
    def op(a1: Boolean, a2: Boolean) = a1 || a2

    def zero = false
  }

  val booleanAnd: Monoid[Boolean] = {
    def op(a1: Boolean, a2: Boolean) = a1 && a2

    def zero = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = {
    val monoid: Monoid[Option[A]] = {
      def op(a1: Option[A], a2: Option[A]) = a1.orElse(a2)
      def zero[A] = None
    }
    monoid
  }

  def endoMonoid[A]: Monoid[A => A] = {
    type Func = A => A
    val monoid: Monoid[Func] = {
      def op(a1: Func, a2: Func): Func = a => a1(a2(a))
      def zero = identity(_)
    }
    monoid
  }

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))
  }

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    foldMap(as, endoMonoid[B])(f.curried)(z)
  }

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    foldMap(as, endoMonoid[B])(a => b => f(a, b))(z)
  }

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.isEmpty) m.zero
    else if (v.length == 1) f(v(0))
    else {
      val halves = v.splitAt(v.length / 2)
      m.op(foldMapV(halves._1, m)(f), foldMapV(halves._2, m)(f))
    }
  }

  def isOrdered(as: IndexedSeq[Int]): Boolean = {
    type Segment = Option[(Int, Int, Boolean)]
    val monoid: Monoid[Segment] = {
      def op(a1: Segment, a2: Segment): Segment = {
        (a1, a2) match {
          case (Some((min1, max1, ordered1)), Some((min2, max2, ordered2))) => Some(Math.min(min1, min2), Math.max(max1, max2), ordered1 && ordered2 && max1 <= min2)
          case (x, None) => x
          case (None, x) => x
        }
      }

      val zero = None
    }
    foldMap(as.toList, monoid)(int => Some(int, int, true)).forall(_._3)
  }
}
