package core

import java.time.OffsetDateTime

case class PostalCode private (zipCode: String)
  object PostalCode {
    def createPostalCode(postalCode: String): Option[PostalCode] = {
      if (postalCode.matches("\\d{5}"))
        Option(new PostalCode(postalCode))
      else
        None
    }
  }

  case class PostalCodeCollection private (postalCodes: Set[PostalCode])

  trait PostalCodeCollectionInterface {
    def emptyPostalCodeCollection: PostalCodeCollection
    def addPostalCode(postalCode: Option[PostalCode], postalCodeCollection: Option[PostalCodeCollection]): Option[PostalCodeCollection]
    def createPostalCodeCollection(postalCodes: Seq[Option[PostalCode]]): Option[PostalCodeCollection]
  }

  object PostalCodeCollection extends PostalCodeCollectionInterface {
    def emptyPostalCodeCollection: PostalCodeCollection = new PostalCodeCollection(Set.empty)
    def addPostalCode(postalCode: Option[PostalCode], postalCodeCollection: Option[PostalCodeCollection]): Option[PostalCodeCollection] = {

      if (
        postalCode.isEmpty
        || postalCodeCollection.isEmpty
        || postalCodeCollection.get.postalCodes.contains(postalCode.get)
        || postalCodeCollection.get.postalCodes.size >= 4)
        None
      else
        Option(new PostalCodeCollection(postalCodeCollection.get.postalCodes + postalCode.get))
    }
    def createPostalCodeCollection(postalCodes: Seq[Option[PostalCode]]): Option[PostalCodeCollection] = {
      postalCodes
        .foldLeft(Option(PostalCodeCollection.emptyPostalCodeCollection))
        ((postalCodeCollection, postalCode) => PostalCodeCollection.addPostalCode(postalCode, postalCodeCollection))
    }
  }

  case class Package private(id:String, weight: Double, postalCode: PostalCode){
    override def equals(obj: Any): Boolean = {
      obj match {
        case packageToCompare: Package => packageToCompare.id == this.id
        case _ => false
      }
    }
  }

  object Package {
    def createPackage(id: String, weight: Double, postalCode: PostalCode, validPostalCodeCollection: PostalCodeCollection): Option[Package] = {
      if (weight > 0 && validPostalCodeCollection.postalCodes.contains(postalCode))
        Option(new Package(id, weight, postalCode))
      else
        None
    }
  }

  case class PackageCollection private (packages: Set[Package])
  trait PackageCollectionInterface {
    def emptyPackageCollection: PackageCollection
    def addPackage(packageToAdd: Option[Package], packageCollection: Option[PackageCollection]): Option[PackageCollection]
    def createPackageCollection(packages: Seq[Option[Package]]): Option[PackageCollection]
  }
  object PackageCollection extends PackageCollectionInterface {
    def emptyPackageCollection: PackageCollection = new PackageCollection(Set.empty)
    def addPackage(packageToAdd: Option[Package], packageCollection: Option[PackageCollection]): Option[PackageCollection] = {
      if (
        packageToAdd.isEmpty
        || packageCollection.isEmpty
        || packageCollection.get.packages.contains(packageToAdd.get))
        None
      else
        Option(new PackageCollection(packageCollection.get.packages + packageToAdd.get))
    }
    def createPackageCollection(packages: Seq[Option[Package]]): Option[PackageCollection] = {
      packages
        .foldLeft(Option(PackageCollection.emptyPackageCollection))
        ((packageCollection, packageToAdd) => PackageCollection.addPackage(packageToAdd, packageCollection))
    }
  }
  case class Person private (name: String)
  object Person {
    def createPerson(name: String): Option[Person] = {
      if (name.matches("[a-zA-Z]+"))
        Option(new Person(name))
      else
        None
    }
  }
  case class DeliveryRoute (packages: Seq[Package], postalCode: PostalCode, deliveryPerson: Person)

  case class DeliveryPlanner (packages: PackageCollection,
                                    postalCodes: PostalCodeCollection,
                                    deliveryPersonLight: Person,
                                    deliveryPersonHeavy: Person)
  object DeliveryPlanner {
    private val _heavyThreshold = 2.0

    def createDeliveryRoutes(deliveryPlanner: DeliveryPlanner): Seq[DeliveryRoute] = {
      val packages = deliveryPlanner.packages.packages.toSeq
      val postalCodes = deliveryPlanner.postalCodes.postalCodes.toSeq
      val deliveryPersonLight = deliveryPlanner.deliveryPersonLight
      val deliveryPersonHeavy = deliveryPlanner.deliveryPersonHeavy

      val lightPackagesToDeliver = packages
        .filter(packageToDeliver => packageToDeliver.weight <= _heavyThreshold)

      val heavyPackagesToDeliver = packages
        .filter(packageToDeliver => packageToDeliver.weight > _heavyThreshold)

      val lightDeliveryRoutes = createDeliveryRoute(postalCodes, deliveryPersonLight, lightPackagesToDeliver)

      val heavyDeliveryRoutes = createDeliveryRoute(postalCodes, deliveryPersonHeavy, heavyPackagesToDeliver)
      val deliveryRoutes = lightDeliveryRoutes ++ heavyDeliveryRoutes
      deliveryRoutes.filter(_.packages.nonEmpty)
    }

    private def createDeliveryRoute(postalCodes: Seq[PostalCode], deliveryPerson: Person, packagesToDeliver: Seq[Package]) = {
      val deliveryRoutes = for postalCode <- postalCodes yield
        val packagesByPostalCode = packagesToDeliver
          .filter(_.postalCode == postalCode)
        DeliveryRoute(packagesByPostalCode, postalCode, deliveryPerson)
      deliveryRoutes.toSeq
    }
  }

  case class Signature ()
  case class DeliveryReceipt private(packageId:String, dni: String, signature: Signature, date: OffsetDateTime, deliveryCode: String)
  object DeliveryReceipt {
    def apply(packageId:String, dni: String, date: OffsetDateTime, deliveryCode: String): DeliveryReceipt = {
      new DeliveryReceipt(packageId, dni,Signature(), date, deliveryCode)
    }
    def apply(packageId:String,signature: Signature, date: OffsetDateTime, deliveryCode: String): DeliveryReceipt = {
      new DeliveryReceipt(packageId, "", signature, date, deliveryCode)
    }
  }



// Dependency injection was used to keep DeliveryRunner.run function pure
  trait GetCurrentTime(){
    def now():OffsetDateTime
  }

  trait ObtainCustomerDni(){
    def get(packageId:String):String
  }

  trait ObtainCustomerPhone(){
      def get():Integer
    }

  trait SMSSender(){
    def send(phoneNumber: Integer, message:String):Unit
  }

  trait LogRegister(){
    def logError(message:String):Unit
  }

  object DeliveryRunner:
    def run(deliveryRoute: DeliveryRoute, getCurrentTime:GetCurrentTime, obtainCustomerDni:ObtainCustomerDni,obtainCustomerPhone:ObtainCustomerPhone, SMSSender: SMSSender, logRegister: LogRegister): Seq[DeliveryReceipt] = {
      val packages = deliveryRoute.packages
      val postalCode = deliveryRoute.postalCode
      val deliveryPerson = deliveryRoute.deliveryPerson

      val deliveryReceipts = for packageToDeliver <- packages yield
        val packageId = packageToDeliver.id
        val deliveryReceipt = DeliveryReceipt(packageToDeliver.id, obtainCustomerDni.get(packageToDeliver.id),getCurrentTime.now(), packageId)
        try {
          SMSSender.send(obtainCustomerPhone.get(),s"The package $packageId has been delivered")
        } catch {
          case e => logRegister.logError(e.getMessage)
        }
        deliveryReceipt
      deliveryReceipts.toSeq
    }
