package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[A]
    y <- oneOf(const(empty),genHeap)
    res <- insert(x, y)
  } yield res


  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("emptyOnDelete") =
     forAll{ i : Int =>
       val h = empty
       isEmpty(deleteMin(insert(i,h)))
     }


  property("minimum of two insert") = forAll{ (a: Int, b: Int) =>
      val h = empty
      findMin(insert(a, insert(b,h))) == Math.min(a,b)
    }

  property("finding minimum after melding") = forAll {
    (h1 : H, h2 : H) =>
      Math.min(findMin(h1),findMin(h2)) == findMin(meld(h1,h2))
  }

  property("finding minimum after melding") = forAll {
    (h1 : H) =>
      findMin(meld(empty,h1)) == findMin(h1)
  }


  property("sorted sequence") = forAll{
    h : H =>

      @tailrec
      def checkSorted ( list : List[Int]) : Boolean = {
        list match {
          case Nil => true
          case _ :: Nil => true
          case head :: tail  =>
            if (head > tail.head )
            false
          else checkSorted(tail)
        }
      }

      checkSorted(giveSortedSeq(h))
   }


  property("create new heap from existing heap should work") = forAll{
     h : H =>
       def createHeap(h: H)  : H = {
         if(isEmpty(h))
           empty
         else{
          val m = findMin(h)
           meld(insert(m, empty), createHeap(deleteMin(h)))
         }
       }

       findMin(h) == findMin(createHeap(h))
  }

  property("insert should work ") = forAll{
     list : Set[Int] =>
      def createHeapFromList(list : List[Int], heap : H) : H = {
        list match {
          case Nil => heap
          case x :: xs => createHeapFromList(xs , insert(x, heap))
        }
      }
      val h = createHeapFromList(list.toList, empty)
      giveSortedSeq(h).toSet == list

  }

  property("is empty ") = forAll{
    _ : H => isEmpty(empty)
  }

  def giveSortedSeq( h : H) : List[Int] = {
    if (isEmpty(h))
      Nil
    else {
      findMin(h) :: giveSortedSeq(deleteMin(h))
    }
  }
}

