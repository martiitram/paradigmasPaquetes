import core.*
import java.time.OffsetDateTime

@main
def main(): Unit = {

  val postalCodeCollection = getPostalCodeCollectionByTerminalInput
  val packageCollection = getPackageCollectionByTerminalInput(postalCodeCollection)
  val lightDeliveryPerson = getPersonByTerminalInput("light")
  val heavyDeliveryPerson = getPersonByTerminalInput("heavy")
  val deliveryRoutes = DeliveryPlanner.createDeliveryRoutes(DeliveryPlanner(packageCollection, postalCodeCollection,
    lightDeliveryPerson, heavyDeliveryPerson))
  val deliveredPackageInformation = for deliveryRoute <- deliveryRoutes yield
    DeliveryRunner.run(deliveryRoute, JavaGetCurrentTime, ConsoleObtainCustomerDni, ConsoleObtainCustomerPhone,
      DummySMSSender, ConsoleLogRegister)
}

def getPostalCodeCollectionByTerminalInput = {
  println("Please input the postal codes separated by spaces.")
  val postalCodes = scala.io.StdIn.readLine().split(" ").toList
  val postalCodeSeq = postalCodes
    .map(PostalCode.createPostalCode(_))
  var postalCodeCollection = PostalCodeCollection.createPostalCodeCollection(postalCodeSeq)
  while postalCodeCollection == None do
    println("The provided inputs are invalid.")
    println("Please input the postal codes separated by spaces.")
    val postalCodesRetry = scala.io.StdIn.readLine().split(" ").toList
    val postalCodeSeqRetry = postalCodesRetry
      .map(PostalCode.createPostalCode(_))
    postalCodeCollection = PostalCodeCollection.createPostalCodeCollection(postalCodeSeqRetry)
  postalCodeCollection.get
}
def getPackageCollectionByTerminalInput(validPostalCodeCollection: PostalCodeCollection) = {
  println("How many packages do you want to introduce?")
  val numberOfPackages = scala.io.StdIn.readLine().toInt
  val packages = (1 to numberOfPackages).map(_ => getPackageByTerminalInput(validPostalCodeCollection))

  var packageCollection = PackageCollection.createPackageCollection(packages)
  while packageCollection.isEmpty do
    println("The provided package collection is invalid.")
    packageCollection = PackageCollection.createPackageCollection(packages)
  packageCollection.get
}
def getPackageByTerminalInput(validPostalCodeCollection: PostalCodeCollection): Option[Package] = {
  println("Please input the packages id.")
  val id = scala.io.StdIn.readLine()
  println("Please input the packages weight.")
  val weight = scala.io.StdIn.readLine().toDouble
  println("Please input the packages postal code.")
  val postalCodeString = scala.io.StdIn.readLine()
  val postalCode = PostalCode.createPostalCode(postalCodeString)
  val package_object = if postalCode.isEmpty then
    None
  else
    Package.createPackage(id, weight, postalCode.get, validPostalCodeCollection)

  if package_object.isEmpty then
    println("The provided inputs are invalid so the package were not created.")
    getPackageByTerminalInput(validPostalCodeCollection)
  else
    package_object
  end if
}

def getPersonByTerminalInput(personType: String): Person = {
  println(s"Please input the $personType delivery person's name.")
  val name = scala.io.StdIn.readLine()
  val person = Person.createPerson(name)
  if person.isEmpty then
    println("The provided inputs are invalid so the person were not created.")
    getPersonByTerminalInput(personType)
  else
    person.get
}

object ConsoleObtainCustomerDni extends ObtainCustomerDni {
  def get(packageId: String): String = {
    println("Please input the DNI of the package receiver.")
    val id = scala.io.StdIn.readLine()
    if id.isEmpty then
      println("The provided inputs are invalid so the package were not created.")
      get(packageId)
    else
      id
    end if
  }
}

object ConsoleObtainCustomerPhone extends ObtainCustomerPhone {
  def get(): Integer = {
    println("Please input the phone number of the package receiver.")
    val id = scala.io.StdIn.readLine()
    if id.isEmpty then
      println("The provided inputs are invalid so the package were not created.")
      get()
    else
      id.toInt
    end if
  }
}

object JavaGetCurrentTime extends GetCurrentTime {
  def now(): OffsetDateTime = OffsetDateTime.now()
}

object DummySMSSender extends SMSSender {
  def send(phoneNumber: Integer, message: String): Unit = {
    println(s"Sending message $message to $phoneNumber")
  }
}

object ConsoleLogRegister extends LogRegister() {
  def logError(message: String): Unit = {
    println(s"An error has ocurred: $message")
  }
}