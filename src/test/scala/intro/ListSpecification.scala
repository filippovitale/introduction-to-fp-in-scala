package intro

import org.scalacheck._, Arbitrary._, Gen._, Prop._

object ListSpecification extends Properties("List") {
  /*
   * Example: Verify that that our Lists.length matches the
   * builtin List#size
   */
  property("Lists#length matches standard library") =
    forAll((xs: List[Int]) => Lists.length(xs) == xs.size)

  /* Exercise 1 */
  property("Lists#append - the length of the result is equal to the sum of the two input lengths") =
    forAll((xs: List[Int], ys: List[Int]) => Lists.length(xs) + Lists.length(ys) == Lists.append(xs, ys).size)

  /* Exercise 2 */
  property("Lists#append - every element in the first and second list appears in the result") =
    forAll((xs: List[Int], ys: List[Int]) => xs.forall(Lists.append(xs, ys).contains) && ys.forall(Lists.append(xs, ys).contains))

  /* Exercise 3 */
  property("Lists#filter - filter(_ => false) always gives empty list") =
    forAll((xs: List[Int]) => Lists.filter(xs)(_ => false) == List.empty)

  /* Exercise 4 */
  property("Lists#filter - filter(_ => true) always gives input list") =
    forAll((xs: List[Int]) => Lists.filter(xs)(_ => true) == xs)

  /* Exercise 5 */
  property("Lists#filter - length of output is always less than length of input") =
    forAll((xs: List[Int], p: Int => Boolean) => Lists.filter(xs)(p).size <= Lists.length(xs))

  /* *Challenge* exercise 6
     Identify a set of properties that together with the type signature
     guarantees the validity of your reverse function (assuming pure-total FP) */
  property("Lists#reverse - the reverse of a reversed list always gives input list") =
    forAll((xs: List[Int]) => Lists.reverse(Lists.reverse(xs)) == xs)

  property("Lists#reverse - the length of a reversed list is equal to the length of the input list") =
    forAll((xs: List[Int]) => Lists.length(Lists.reverse(xs)) == Lists.length(xs))

  /* *Challenge* exercise 7
     Identify a set of properties for testing sequence */
  property("Lists#sequence - the length of the sequence that is not None is equal to the length of the input list") =
    forAll((xs: List[Option[Int]]) => Lists.sequence(xs).forall(l => Lists.length(l) == Lists.length(xs)))

  property("Lists#sequence - the sequence of a list that doesn't contain Nones is not None") =
    forAll((xs: List[Int]) => Lists.sequence(xs.map(Option(_))).isDefined)

  /* *Challenge* exercise 8
     Identify a set of properties for testing ranges */
  property("Lists#ranges - the range must contains elements from the original list") =
    forAll((xs: List[Int]) => Lists.ranges(xs).flatMap { case (a, b) => List(a, b) }.forall(xs.contains))

}
