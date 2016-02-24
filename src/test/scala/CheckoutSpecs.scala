import com.ferran.Checkout.Basket
import com.ferran.{Checkout, Orange, Apple, Item}
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

  """Checkout""" should {

    """Calculate price from empty basket properly""" in {
      val test:Basket = Seq.empty[Item]
      Checkout.checkPrice(test) === 0
    }

    """Calculate price from basket properly no offers to apply""" in {
      val test:Basket = Seq(Apple, Orange, Orange)
      Checkout.checkPrice(test) === 1.10
    }

    """Calculate single Apple offer price""" in {
      val test:Basket = Seq(Apple, Apple)
      Checkout.checkPrice(test) === 0.6
    }

    """Calculate multiple Apple offers""" in {
      val test:Basket = Seq(Apple, Apple, Apple, Apple, Apple)
      Checkout.checkPrice(test) === 1.8
    }

    """Calculate single Orange offer price""" in {
      val test:Basket = Seq(Orange, Orange, Orange)
      Checkout.checkPrice(test) === 0.5
    }

    """Calculate multiple Orange offers""" in {
      val test:Basket = Seq(Orange, Orange, Orange, Orange, Orange, Orange, Orange)
      Checkout.checkPrice(test) === 1.25
    }

    """Calculat mix of offers""" in {
      val test:Basket = Seq(Orange, Apple, Orange, Apple, Orange)
      Checkout.checkPrice(test) === 1.1
    }
  }
}
