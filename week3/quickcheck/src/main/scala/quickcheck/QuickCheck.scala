package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Prop.BooleanOperators

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(const(empty), for {
    random <- arbitrary[Int]
    heap <- oneOf(const(empty), genHeap)
  } yield insert(random, heap))

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("when inserting the minimum value, minimum remains constant") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("adding a property and removing the minimum yields an empty heap") = forAll { (h: H, number: Int) =>
    isEmpty(h) ==> isEmpty(deleteMin(insert(number, h)))
  }

  property("inserting two elements inside an array, yields as minimum the min of the two elements") = forAll { (h: H, numOne: Int, numTwo: Int) =>
    isEmpty(h) ==> {
      val min = findMin(insert(numOne, insert(numTwo, h)))
      min == math.min(numOne, numTwo)
    }
  }

  property("min of two combined heaps is the min of the mins") = forAll { (hOne: H, hTwo: H) =>
    val minOne = if (isEmpty(hOne)) Int.MaxValue else findMin(hOne)
    val minTwo = if (isEmpty(hTwo)) Int.MaxValue else findMin(hTwo)
    val melded = meld(hOne, hTwo)
    val minCombined = if (isEmpty(melded)) Int.MaxValue else findMin(melded)
    minCombined == math.min(minOne, minTwo)
  }

  property("given any heap when removing the minimum, it should return a sorted collection") = forAll { (h: H) =>
    isSorted(h)
  }

  def isSorted(h: H): Boolean = {
    if (isEmpty(h)) {
      true
    } else {
      val minimum = findMin(h)
      val tail = deleteMin(h)
      if (isEmpty(tail)) {
        true
      } else {
        ord.lteq(minimum, findMin(tail)) && isSorted(tail)
      }
    }
  }

  property("meld of 2 heaps should be itself sorted") = forAll { (hOne: H, hTwo: H) =>
      val melded = meld(hOne, hTwo)
      isSorted(melded)
  }

  property("adding one element to a list of one element") = forAll { (h: H) =>
    (getMin(h) != Int.MinValue) ==> {
      val heapMin = getMin(h)
      val max = heapMin - 1
      val min = max - 1
      val heapWithMin = insert(min, h)
      val heapWithMax = insert(max, heapWithMin)
      val heapWithOnlyMax = deleteMin(heapWithMax)
      val shouldBeMax = findMin(heapWithOnlyMax)
      shouldBeMax == max
    }
  }

  private def getMin(heap: H) = if (isEmpty(heap)) Int.MaxValue else findMin(heap)
}
