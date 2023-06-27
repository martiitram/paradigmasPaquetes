import core.*
@main
def main(): Unit = {
  println("Please input the postal codes separated by spaces.")
  val postalCodes = scala.io.StdIn.readLine().split(" ").toList
  val postalCodeSeq = postalCodes
    .map(PostalCode.createPostalCode(_))
  val postalCodeCollection = PostalCodeCollection.createPostalCodeCollection(postalCodeSeq)
  //TODO: Add packages

  //TODO: Start route of person

    //TODO: Delivery package

  println(postalCodeCollection)
}