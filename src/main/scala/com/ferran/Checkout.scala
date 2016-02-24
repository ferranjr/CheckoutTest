package com.ferran


// Items can be added to a basket
sealed abstract class Item( val price: BigDecimal )
case object Apple extends Item(0.6)
case object Orange extends Item(0.25)

object Item {

  /**
   * Parse Input String to Item, if valid string
   *
   * @param in string to be parsed
   * @return
   */
  def fromString(in: String): Option[Item] =
    in match {
      case "apple" => Some(Apple)
      case "orange" => Some(Orange)
      case _ => None
    }
}

object Checkout {

  // Basket representation
  type Basket = Seq[Item]

  object Basket{
    def apply(arr: Array[String]): Seq[Item] = {
      arr.map(Item.fromString)
        .filter(_.isDefined)
        .map(_.get)
    }
  }

  /**
   * Calculates price given a Basket
   * @param in the basket to checkout
   * @return
   */
  def checkPrice(in: Basket): BigDecimal = {
    in.foldLeft(BigDecimal(0))( (acc, item) => acc + item.price )
  }


  def main(args: Array[String]): Unit = {
    println(
      s"""
         |Basket Total: ${checkPrice(Basket(args))}
       """.stripMargin)
  }
}
