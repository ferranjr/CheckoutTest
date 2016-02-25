# CheckoutTest
HMRC Scala Test

## Assumptions
1. When passing the args to main, if some string parameters don't match to a name of the valid items, I ignored that item but not invalidate the rest of the list.

```Scala
  scala> com.ferran.Checkout.main(Array("orange","orange","apple","foo"))
  Basket Total: 1.10
```

