package laziness

sealed trait Stream[+A] {
  def toList = Stream.toList(this)

  def take = Stream.take(this)(_)

  def takeWhile = Stream.takeWhile(this)(_)

  def drop = Stream.drop(this)(_)
}

case object Empty extends Stream[Nothing]

case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](head: => A, tail: => Stream[A]): Stream[A] = {
    lazy val hd = head
    lazy val tl = tail
    Cons(() => hd, () => tl)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))

  def toList[A](stream: Stream[A]): List[A] = stream match {
    case Empty => List.empty
    case Cons(head, tail) => List(head()) ++ toList(tail())
  }

  def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match {
    case (Empty, _) => Empty
    case (_, remains) if remains == 0 => Empty
    case (Cons(head, tail), remains) => cons(head(), take(tail())(remains -1))
  }

  def drop[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match {
    case (Empty, _) => Empty
    case (s, remains) if remains == 0 => s
    case (Cons(head, tail), remains) => drop(tail())(remains - 1)
  }

  def takeWhile[A](stream: Stream[A])(p: A => Boolean): Stream[A] = stream match {
    case Empty => Empty
    case Cons(head, tail) => if (p(head())) cons(head(), takeWhile(tail())(p)) else Empty
  }

  def exist[A](stream: Stream[A])(p: A => Boolean): Boolean = stream match  {
    case Empty => false
    case Cons(head, tail) => p(head()) || exist(tail())(p)
  }

  def forAll[A](stream: Stream[A])(p: A => Boolean): Boolean = stream match {
    case Empty => true
    case Cons(head, tail) => p(head()) && forAll(tail())(p)
  }

  def foldRight[A, B](stream: Stream[A])(z: => B)(f: (A, => B) => B): B = stream match {
    case Empty => z
    case Cons(head, tail) => foldRight(tail())(f(head(), z))(f)
  }

  def headOption[A](stream: Stream[A]): Option[A] = foldRight(stream)(Option.empty[A])((cur, prev) => prev.orElse(Option(cur)))

  def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = foldRight(stream)(Empty[B])((cur, prev) => cons(f(cur), prev))

  def filter[A](stream: Stream[A])(f: A => Boolean): Stream[A] = foldRight(stream)(Empty[A])((cur, prev) =>
  if (f(cur)) cons(cur, prev) else prev)

  def append[A](stream:Stream[A])(ap: Stream[A]): Stream[A] = foldRight(stream)(ap)((cur, prev) => cons(cur, prev))

  def flatMap[A, B](stream: Stream[A])(f: A => Stream[B]): Stream[B] = foldRight(stream)(Empty[B])((cur, prev) => append(f(cur))(prev))
}