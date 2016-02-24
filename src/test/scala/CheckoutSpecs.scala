import com.ferran.Checkout.Basket
import com.ferran.{Orange, Apple, Item}
import org.specs2.mutable.Specification

class CheckoutSpecs extends Specification {

  """Item""" should {

    """Create item from a single valid String""" in {
      val stringApple = "apple"
      val stringOrange = "orange"
      val stringUnknown = "unkown"

      Item.fromString(stringApple) === Some(Apple)
      Item.fromString(stringOrange) === Some(Orange)
      Item.fromString(stringUnknown) === None
    }
  }

  """Basket""" should {

    """Create a proper basket from and empty array""" in {
      val test: Array[String] = Array.empty[String]
      Basket(test) === Seq.empty[Item]
    }

    """Create a proper basket from all valid strings""" in {
      val test: Array[String] = Array("apple", "orange", "apple")
      Basket(test) === Seq(Apple, Orange, Apple)
    }

    """Create a proper basket with only the valid strings, ignore rest""" in {
      val test: Array[String] = Array("apple", "orange", "apple", "unkown")
      Basket(test) === Seq(Apple, Orange, Apple)
    }
  }
}
