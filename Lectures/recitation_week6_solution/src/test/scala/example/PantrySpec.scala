package example
import org.scalatest.flatspec.AnyFlatSpec

class PantrySpec extends AnyFlatSpec {

  // Anatomy of a test:
  /**
    * <functionality we're testing> should <what we expect to occur> in {
    *   <body of test>
    * }
    */
  "loading JSON" should "load JSON properly" in {
    val base = List(Item("burger", 10))
    val res = Pantry.processCommand("j",base)("test_pantry.json")
    assert(res.length == 3)
    assert(res(0).name == "salsa")
  }

  "sorting list" should "not change order" in {
    val base = List(Item("tiropita", 60), Item("burger", 100))
    val res = Pantry.processCommand("l",base)("n")
    assert(res == base)
  }

  "updating an item" should "update the item" in {
    val base = List(Item("tiropita", 60), Item("burger", 100))
    val res = Pantry.processCommand("0",base)("100")
    assert(res(0).name == "tiropita" && res(0).quantity == 100)
  }
}
