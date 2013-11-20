package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math._


abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  // Inserting an int in a empty heap should have that int as the min of the heap
  // (it is the only element)
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  lazy val genHeap: Gen[H] = for {
    n <- arbitrary[A]
    h <- frequency((1, value(empty)), (9, genHeap))
  } yield insert(n, h)

  // Checks if findMin is working correctly
  property("equalMinJoinOrDistincHeaps") = forAll { (n1: A, n2: A) =>
    val h = insert(n1, insert(n2, empty))
    findMin(h) == min(n1, n2)
  }

  // Checks if meld (union) is working corretly
  property("checkEquality") = forAll { (h1: H, h2: H) =>
    def heapEqual(h1: H, h2: H): Boolean =
      // Both heaps are empty
      if (isEmpty(h1) && isEmpty(h2))
        true

      else {
        // Now let's check if both heaps have the minimum, recursivly until they are empty
        val min1 = findMin(h1)
        val min2 = findMin(h2)
        min1 == min2 && heapEqual(deleteMin(h1), deleteMin(h2))
      }

    // Check if melding heaps A U B is equal to merging A - min(A) U B + min(A)
    val testHeap1 = meld(h1, h2);
    val testHeap2 = meld(deleteMin(h1), insert(findMin(h1), h2))
    heapEqual(testHeap1, testHeap2)
  }

  // Creates an Arbitrary instance
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
}
