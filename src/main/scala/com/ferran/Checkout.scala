package com.ferran


// Items can be added to a basket
sealed abstract class Item( val price: BigDecimal ){

  type ItemsToQualify = Int
  type ItemsToDiscount = Int
  type Offer = (ItemsToQualify, ItemsToDiscount)

  /**
   * Represents offer to apply with this item
   * @return
   */
  def offer:Offer
}
case object Apple extends Item(0.6){
  /**
   * Apple's offer:
   * Buy One Get One Free
   */
  val offer:Offer = (2,1)
}
case object Orange extends Item(0.25){
  /**
   * Orange's offer:
   * Get 3 for the price of 2
   */
  val offer:Offer = (3,1)
}

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
