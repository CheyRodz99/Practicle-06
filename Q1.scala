import scala.io.StdIn.readLine

object Q1 extends App{

  val inventory1: Map[Int, (String, Int, Double)] = Map(
    101 -> ("Item A", 10, 25.5),
    102 -> ("Item B", 5, 30.0),
    103 -> ("Item C", 20, 15.0)
  )

  val inventory2: Map[Int, (String, Int, Double)] = Map(
    102 -> ("Item B", 3, 32.0),
    104 -> ("Item D", 7, 22.0),
    105 -> ("Item E", 10, 12.5)
  )

  val productNames: Iterable[String] = inventory1.values.map(_._1)
  println("Product Names in inventory1: " + productNames.mkString(", "))
  println()


  val totalValue: Double = inventory1.values.map { case (_, quantity, price) => quantity * price }.sum
  println("Total Value of inventory1: Rs." + totalValue)
  println()


  val isEmpty: Boolean = inventory1.isEmpty
  println("Is inventory1 empty? " + isEmpty)
  println()


  val mergedInventory: Map[Int, (String, Int, Double)] = inventory1 ++ inventory2.map {
    case (id, (name, quantity, price)) =>
      if (inventory1.contains(id)) {
        val (existingName, existingQuantity, existingPrice) = inventory1(id)
        id -> (existingName, existingQuantity + quantity, Math.max(existingPrice, price))
      } else {
        id -> (name, quantity, price)
      }
  }

  println("Merged Inventory: ")
  mergedInventory.foreach { case (id, (name, quantity, price)) =>
    println(s"ID: $id, Name: $name, Quantity: $quantity, Price: $price")
  }
  println()


  print("Enter the product ID to check: ")
  val productIdToCheck: Int = readLine().toInt
  println(s"Checking for product ID: $productIdToCheck")

  mergedInventory.get(productIdToCheck) match {
    case Some((name, quantity, price)) =>
      println(s"Product ID: $productIdToCheck found!")
      println(s"Name: $name, Quantity: $quantity, Price: $price")
    case None =>
      println(s"Product ID: $productIdToCheck not found in inventory1.")
  }

}
