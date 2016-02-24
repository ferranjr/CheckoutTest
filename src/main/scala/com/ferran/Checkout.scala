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

  /**
   * Represents name of the item type
   * @return
   */
  def name:String
}
case object Apple extends Item(0.6){
  /**
   * Apple's offer:
   * Buy One Get One Free
   */
  val offer:Offer = (2,1)

  val name: String = "apple"
}
case object Orange extends Item(0.25){
  /**
   * Orange's offer:
   * Get 3 for the price of 2
   */
  val offer:Offer = (3,1)

  val name: String = "orange"
}

object Item {

  /**
   * Parse Input String to Item, if valid string
   *
   * @param in string to be parsed
   * @return
   */
  def fromString(in: String): Option[Item] =
    in.toLowerCase match {
      case Apple.name   => Some(Apple)
      case Orange.name  => Some(Orange)
      case _            => None
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
   * Given a sequence of same type of items and the info about offer and price,
   * returns the final price for that type of items
   *
   * @param in sequence of items to check price with offer
   * @param price price for that type if items
   * @param offer offer to apply to that type of item, if possible
   * @return
   */
  def getPriceWithOffer(in: Seq[Item], price: BigDecimal, offer: Item#Offer): BigDecimal = {
    val itemsToDiscount = (in.length/offer._1) * offer._2
    (in.length - itemsToDiscount) * price
  }


  /**
   * Calculates price given a Basket
   * @param in the basket to checkout
   * @return
   */
  def checkPrice(in: Basket): BigDecimal = {
    in
      .groupBy{
        case Apple  => Apple.name
        case Orange => Orange.name
      }
      .foldLeft(BigDecimal(0)){
        case (acc, (Apple.name, apples))   => acc + getPriceWithOffer(apples, Apple.price, Apple.offer)
        case (acc, (Orange.name, oranges)) => acc + getPriceWithOffer(oranges, Orange.price, Orange.offer)
      }
  }


  /**
   * Main function to test the application
   * @param args
   */
  def main(args: Array[String]): Unit = {
    println(
      s"""
         |Basket Total: ${checkPrice(Basket(args))}
       """.stripMargin)
  }
}
