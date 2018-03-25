import org.scalatest.FlatSpec

class MapToPersonSpec extends FlatSpec {

  case class Person(name: String, age: Int, phone: String)

  def map2List (m: Map[Int, (String, Int, String)]): List[Person] = {
    m.values.map(x => Person(x._1, x._2, x._3)).toList.sortBy(_.age)
  }

  val testedFunc: Map[Int, (String, Int, String)] => List[Person] = map2List

  val data = Map(
    1 -> ("Bob", 30, "+70001112234"),
    2 -> ("Alex", 26, "+70001112233"),
    3 -> ("John", 50, "+70001112235"),
    4 -> ("Dan", 55, "+70001112236"),
  )

  val expected = List(
    Person("Alex", 26, "+70001112233"),
    Person("Bob", 30, "+70001112234"),
    Person("John", 50, "+70001112235"),
    Person("Dan", 55, "+70001112236")
  )

  "Map of tuples" should "convert to list of persons" in {
    assert(testedFunc(data).sortBy(_.age) == expected)
  }
}
