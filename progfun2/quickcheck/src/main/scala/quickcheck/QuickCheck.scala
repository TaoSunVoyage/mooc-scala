package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
      const(empty),
      for {
          x <- arbitrary[Int]
          h <- oneOf(const(empty), genHeap)
      } yield insert(x, h)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { (a: A) =>
      val h = insert(a, empty)
      findMin(h) == a
  }

  property("min2") = forAll { (a: A, b: A) =>
      val h = insert(b, insert(a, empty))
      findMin(h) == (if (ord.lteq(a, b)) a else b)
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("add_delete1") = forAll { (a: A) =>
      val h = deleteMin(insert(a, empty))
      isEmpty(h)
  }

  property("add_delete2") = forAll { (a: A, b: A) =>
      val h = deleteMin(insert(b, insert(a, empty)))
      findMin(h) == (if (ord.lteq(a, b)) b else a)
  }

  property("add_delete3") = forAll { (a: A, b: A, c: A) =>
      val h1 = insert(a, insert(b, insert(c, empty)))
      val h2 = insert(c, insert(b, insert(a, empty)))
      findMin(deleteMin(h1)) == findMin(deleteMin(h2))
  }

  property("ordering") = forAll { (h: H) =>
      def check(h: H): Boolean =
          if (isEmpty(h)) true
          else {
              val hh = deleteMin(h)
              if (isEmpty(hh)) true
              else if (ord.lteq(findMin(h), findMin(hh))) check(hh)
              else false
          }
      check(h)
  }

  property("meld_min") = forAll { (h1: H, h2: H) =>
      val hmeld = meld(h1, h2)
      if (isEmpty(h1) && isEmpty(h2)) isEmpty(hmeld)
      else if (isEmpty(h1)) !isEmpty(hmeld) && findMin(hmeld) == findMin(h2)
      else if (isEmpty(h2)) !isEmpty(hmeld) && findMin(hmeld) == findMin(h1)
      else {
          val h1min = findMin(h1)
          val h2min = findMin(h2)
          !isEmpty(hmeld) && findMin(hmeld) == (if (ord.lteq(h1min, h2min)) h1min else h2min)
      }
  }
}
