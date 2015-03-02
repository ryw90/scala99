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

  def encode[V](l: List[V]): List[(Int, V)] =
    pack(l).map { ls => (ls.length, ls.head) }

  def encodeModified[V](l: List[V]): List[Any] = {
    pack(l).map { ls =>
      val length = ls.length
      if(length > 1) (length, ls.head) else ls.head
    }
  }

  def decode[V](l: List[(Int, V)]): List[V] =
    l.flatMap { case (n, elem) => List.fill(n)(elem) }

  def encodeDirect[V](l: List[V]): List[(Int, V)] = l match {
    case Nil => Nil
    case xs: List[_] => {
      val (cur, rest) = xs.span { _ == xs.head }
      (cur.length, cur.head) :: encodeDirect(rest)
    }
  }

  def duplicate[V](l: List[V]): List[V] = l.flatMap { x => List(x, x) }
  def duplicateN[V](n: Int, l: List[V]) = l.flatMap { x => List.fill(n)(x) }

  def drop[V](n: Int, l: List[V]): List[V] = {
    val iterable = for(
      i <- 0 to (l.length - 1) if (i + 1) % n != 0
    ) yield l(i)
    iterable.toList
  }

  def split[V](at: Int, l: List[V]): (List[V], List[V]) = l.splitAt(at)
  /*
  def split[V](at: Int, l: List[V]): (List[V], List[V]) = {
    def splitHelper(rem: Int, left: List[V], right: List[V]): (List[V], List[V]) = {
      if(rem == 0) {
        (left.reverse, right)
      } else {
        splitHelper(rem - 1, right.head :: left, right.tail)
      }
    }
    splitHelper(at, Nil, l)
  }
  */

  def slice[V](i: Int, k: Int, l: List[V]): List[V] = l.drop(i).take(k-i)

  def rotate[V](n: Int, l: List[V]): List[V] = {
    if(n >= 0) l.drop(n) ::: l.take(n) else rotate(n + l.length, l)
  }

  def removeAt[V](k: Int, l: List[V]): (List[V], V) = {
    val (head, tail) = l.splitAt(k)
    (head.take(k) ::: tail.tail, tail.head)
  }
  def insertAt[V](elem: V, k: Int, l: List[V]) = {
    val (head, tail) = l.splitAt(k)
    head ::: elem :: tail
  }

  def range(l: Int, u: Int): List[Int] = {
    def rangeHelper(uCur: Int, res: List[Int]): List[Int] = {
      if (uCur < l) res
      else rangeHelper(uCur - 1, uCur :: res)
    }
    rangeHelper(u, Nil)
  }

  import scala.util.Random
  def randomSelect[V](n: Int, l: List[V]): List[V] = {
    if(n <= 0) Nil
    else {
      val (rest, elem) = removeAt((new Random).nextInt(l.length), l)
      elem :: randomSelect(n - 1, rest)
    }
  }

  def main(args: Array[String]) = (new TestSuite).execute()
}

import Scala99._

class TestSuite extends FunSpec {
  val genList = List(1, 1, 2, 3, 5, 8)
  val dupList = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  val dupListEncoded = List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
  val ordList = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)

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

  describe("P10: encode") {
    it("Generates the run-length encoding of a list") {
      assert(encode(dupList) == dupListEncoded)
    }
  }

  describe("P11: encodeModified") {
    it("Generates the run-length encoding of a list, but only for duplicates") {
      val expected = List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e))
      assert(encodeModified(dupList) == expected)
    }
  }

  describe("P12: decode") {
    it("Decodes a run-length encoded list") {
      assert(decode(dupListEncoded) == dupList)
    }
  }

  describe("P13: encodeDirect") {
    it("Generates the run-length encoding of a list (directly)") {
      assert(encodeDirect(dupList) == dupListEncoded)
    }
  }

  describe("P14: duplicate") {
    it("Duplicates the elements of a list") {
      val list = List('a, 'b, 'c, 'c, 'd)
      assert(duplicate(list) == List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
    }
  }

  describe("P15: duplicateN") {
    it("Duplicates the elements of a list a given number of times") {
      val list = List('a, 'b, 'c, 'c, 'd)
      val expected = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
      assert(duplicateN(3, list) == expected)
    }
  }

  describe("P16: drop") {
    it("Drops every nth element from a list") {
      val expected = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
      assert(drop(3, ordList) == expected)
    }
  }

  describe("P17: split") {
    it("Splits a list into two parts") {
      val expected = (List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
      assert(split(3, ordList) == expected)
    }
  }

  describe("P18: slice") {
    it("Extracts a slice (from ith up to but not including kth elem) from a list") {
      val expected = List('d, 'e, 'f, 'g)
      assert(slice(3, 7, ordList) == expected)
    }
  }

  describe("P19: rotate") {
    it("Rotates when N is positive") {
      val expected = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
      assert(rotate(3, ordList) == expected)
    }

    it("Rotates when N is negative") {
      val expected = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
      assert(rotate(-2, ordList) == expected)
    }
  }

  val abcd = List('a, 'b, 'c, 'd)
  describe("P20: removeAt") {
    it("Removes the kth element from a list, returns list and removed elem") {
      assert(removeAt(1, abcd) == (List('a, 'c, 'd), 'b))
    }
  }

  describe("P21: insertAt") {
    it("Inserts an element at a given position into a list") {
      assert(insertAt('new, 1, abcd) == List('a, 'new, 'b, 'c, 'd))
    }
  }

  describe("P22: range") {
    it("Creates a list containing all integers within a given range") {
      assert(range(4, 9) == List(4, 5, 6, 7, 8, 9))
    }
  }

  describe("P23: randomSelect") {
    it("Extracts a given number of randomly selected elements from a list") {
      import scala.util.Random
      val rng = new Random
      val length = ordList.length
      (1 to 100).foreach { i =>
        val n = rng.nextInt(length - 1)
        val res = randomSelect(n, ordList)
        assert(res.length == n)
        res.foreach { e => assert(res.contains(e)) }
        assert(res.distinct == res)
      }
    }
  }
}
