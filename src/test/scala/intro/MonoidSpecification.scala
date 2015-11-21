package intro

import org.scalacheck._, Arbitrary._, Gen._, Prop._

object MonoidSpecification extends Properties("Monoid") {
  /*
   * Exercise 1: Define arbitrary instances for these basic monoid data types.
   */

  implicit def MinArbitrary: Arbitrary[Min] =
    Arbitrary(arbitrary[Int].map(Min))

  implicit def MaxArbitrary: Arbitrary[Max] =
    Arbitrary(arbitrary[Int].map(Max))

  implicit def FirstArbitrary: Arbitrary[First[Int]] =
    //Arbitrary(arbitrary[Int].map(n => First[Int](Option(n))))
    Arbitrary(arbitrary[Option[Int]].map(First[Int]))

  /*
   * *Challenge* Exercise 2: Define arbitrary instance for Endo.
   *
   * Hint: Use Gen.choose(n: Int, m: Int): Gen[Int]
   * Hint: Use Gen.oneOf[A](values: A*): Gen[A]
   */
  implicit def EndoArbitrary: Arbitrary[Endo[Int]] =
    Arbitrary(arbitrary[Int => Int].map(n => Endo[Int](n)))

  /*
   * *Challenge* Exercise 2: ensure that our Monoid instances satisfy
   * the monoid laws.
   *
   * (A subset has been specified to keep things smaller)
   *
   * Note: Try refactoring as you go, don't repeat the test for
   *       every property.
   */

  def associative[A](implicit M: Monoid[A], arb: Arbitrary[A]) = forAll((a: A, b: A, c: A) =>
    Monoid[A].op(a, Monoid[A].op(b, c)) == Monoid[A].op(Monoid[A].op(a, b), c)
  )

  def rightIdentity[A](implicit M: Monoid[A], arb: Arbitrary[A]) = forAll((a: A) =>
    Monoid[A].op(a, Monoid[A].identity) == a
  )

  def leftIdentity[A](implicit M: Monoid[A], arb: Arbitrary[A]) = forAll((a: A) =>
    Monoid[A].op(Monoid[A].identity, a) == a
  )

  property("Min is Lawful") = all(associative[Min], rightIdentity[Min], leftIdentity[Min])

  property("Max is Lawful") = all(associative[Max], rightIdentity[Max], leftIdentity[Max])

  property("Endo is Lawful") = forAll((a: Endo[Int], b: Endo[Int], c: Endo[Int], i: Int) =>
    Monoid[Endo[Int]].op(a, Monoid[Endo[Int]].op(b, c)).f(i)
      == Monoid[Endo[Int]].op(Monoid[Endo[Int]].op(a, b), c).f(i) &&
    Monoid[Endo[Int]].op(a, Monoid[Endo[Int]].identity).f(i) == a.f(i) &&
    Monoid[Endo[Int]].op(Monoid[Endo[Int]].identity, a).f(i) == a.f(i)
  )

  property("First is Lawful") = all(associative[First[Int]], rightIdentity[First[Int]], leftIdentity[First[Int]])

  property("List is Lawful") = all(associative[List[Int]], rightIdentity[List[Int]], leftIdentity[List[Int]])

  property("Map is Lawful") =  all(associative[Map[String, Min]], rightIdentity[Map[String, Min]], leftIdentity[Map[String, Min]])


  /**
   * *Challenge* Exercise 3: Can you use a property to prove something isn't a Monoid?
   *
   * Hint: What about doubles?
   */
  case class Sub(n: Int)

  implicit def SubMonoid: Monoid[Sub] = new Monoid[Sub] {
    override def identity: Sub = Sub(0)
    override def op(x: Sub, y: Sub): Sub = Sub(x.n - y.n)
  }

  implicit def SubArbitrary: Arbitrary[Sub] =
    Arbitrary(arbitrary[Int].map(Sub))

  property("Not a monoid") = atLeastOne(associative[Sub], rightIdentity[Sub], leftIdentity[Sub])

}
