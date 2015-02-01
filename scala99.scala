import org.scalatest._

object Scala99 {

  def lastNth[V](n: Int, l: List[V]): V = {
    if(n <= 0) throw new IllegalArgumentException
    if(n > l.length) throw new NoSuchElementException
    l.takeRight(n).head
  }

  def last[V](l: List[V]): V = lastNth(1, l)
  def penultimate[V](l: List[V]): V = lastNth(2, l)

  def nth[V](n: Int, l: List[V]): V = {
    if(n >= 0) l(n)
    else throw new NoSuchElementException
  }

  def length[V](l: List[V]): Int = {
    def lengthHelper(acc: Int, l: List[V]): Int = {
      l match {
        case Nil => acc
        case _ :: xs => lengthHelper(acc + 1, xs)
      }
    }
    lengthHelper(0, l)
  }

  def reverse[V](l: List[V]): List[V] = {
    l.foldLeft(Nil: List[V]) { (xs, x) => x :: xs }
  }
  /*
  def reverse[V](l: List[V]): List[V] = {
    def reverseHelper(result: List[V], curList: List[V]): List[V] = curList match {
      case Nil => result
      case x :: xs => reverseHelper(x :: result, xs)
    }
    reverseHelper(Nil, l)
  }
  */

  def isPalindrome[V](l: List[V]): Boolean = (l == l.reverse)

  def flatten(l: List[Any]): List[Any] = l.flatMap {
    case ls: List[_] => flatten(ls)
    case x => List(x)
  }

  def compress[V](l: List[V]): List[V] = {
    l.foldRight(List[V]()) { (x, acc) =>
      if(acc.isEmpty || x != acc.head) x :: acc
      else acc
    }
  }
  /*
  def compress[V](l: List[V]): List[V] = {
    def compressHelper[V](result: List[V], curList: List[V]): List[V] = curList match {
      case x :: xs => compressHelper(x :: result, curList.dropWhile { _ == x })
      case Nil => result.reverse
    }
    compressHelper(Nil, l)
  }
  */

  def pack[V](l: List[V]): List[List[V]] = {
    l.foldRight(List[List[V]]()) { (x, acc) =>
      if(acc.isEmpty || !acc.head.contains(x)) List(x) :: acc
      else (x :: acc.head) :: acc.tail
    }
  }
  /*
  def pack[V](l: List[V]): List[List[V]] = l match {
    case Nil => List[List[V]]()
    case x :: xs => {
      val (seq, rem) = l.span { _ == x }
      seq :: pack(rem)
    }
  }
  */

  def main(args: Array[String]) = (new TestSuite).execute()
}

import Scala99._

class TestSuite extends FunSpec {
  val genList = List(1, 1, 2, 3, 5, 8)
  val dupList = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

  describe("P01: last") {
    it("Finds the last element of a list") {
      assert(last(genList) == 8)
    }
  }

  describe("P02: penultimate") {
    it("Finds the last but one element of a list") {
      assert(penultimate(genList) == 5)
    }
  }

  describe("P03: nth") {
    it("Finds the nth element of a list") {
      assert(nth(2, genList) == 2)
    }
  }

  describe("P04: length") {
    it("Finds the number of elements in a list") {
      assert(length(genList) == 6)
    }
  }

  describe("P05: reverse") {
    it("Reverses a lest") {
      assert(reverse(genList) == List(8, 5, 3, 2, 1, 1))
    }
  }

  describe("P06: isPalindrome") {
    it("Finds out whether a list is a palindrome") {
      assert(isPalindrome(List(1, 2, 3, 2, 1)))
      assert(!isPalindrome(genList))
    }
  }

  describe("P07: flatten") {
    it("Flattens a nested list structure") {
      assert(flatten(List(List(1, 1), 2, List(3, List(5, 8)))) == genList)
    }
  }

  describe("P08: compress") {
    it("Eliminates consecutive duplicates of list elements") {
      val expected = List('a, 'b, 'c, 'a, 'd, 'e)
      assert(compress(dupList) == expected)
    }
  }

  describe("P09: pack") {
    it("Packs consecutive duplicates of list elements into sublists") {
      val expected = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
      assert(pack(dupList) == expected)
    }
  }

}
