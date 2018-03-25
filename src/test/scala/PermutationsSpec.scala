import org.scalatest.FlatSpec

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class PermutationsSpec extends FlatSpec {

  def swap(ab:ArrayBuffer[Int], i:Int, j:Int ) = {
    val s: Int = ab(i)
    ab(i) = ab(j)
    ab(j) = s
  }

  def getNextPermutation(list:List[Int]): List[Int] = {
    val ab:ArrayBuffer[Int] = ArrayBuffer(list : _*)

    var j = ab.length - 2

    while(j >= 0 && ab(j) >= ab(j + 1))
      j = j - 1

    if (j < 0)
      List.empty
    else{
      var k = ab.length - 1

      while(ab(j) >= ab(k))
        k = k - 1

      swap(ab, j, k)

      var l = j + 1
      var r = ab.length - 1

      while (l < r){
        swap(ab, l, r)
        l = l + 1
        r = r - 1
      }

      ab.toList
    }
  }

  def getPermutaions(list:List[Int]) : List[List[Int]] = {
    val lb:ListBuffer[List[Int]] = ListBuffer.empty
    lb.append(list.sorted)
    var newList:List[Int] = List.empty

    do{
      newList = getNextPermutation(lb.last)
      if (newList.nonEmpty)
        lb.append(newList)
    }while (newList.nonEmpty)

    lb.toList
  }

  val testedFunc: List[Int] => List[List[Int]] = getPermutaions

  def compare(actual: List[List[Int]], expected: List[Int]): Boolean = {
    actual.diff(expected.permutations.toList).isEmpty && expected.permutations.toList.diff(actual).isEmpty
  }

  "My permutation func" should "work right for empty list" in {
    val xs = List.empty[Int]
    assert(compare(testedFunc(xs), xs))
  }

  "My permutation func" should "work right for non-empty list" in {
    val xs = List(1, 2, 3, 4)
    assert(compare(testedFunc(xs), xs))
  }

  "My permutation func" should "work right for non-empty list with equal elements" in {
    val xs = List(1, 1, 3, 4)
    assert(compare(testedFunc(xs), xs))
  }
}
