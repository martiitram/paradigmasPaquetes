package core
  case class PostalCode private (zipCode: String)
  object PostalCode {
    def createPostalCode(postalCode: String): Option[PostalCode] = {
      if (postalCode.matches("\\d{5}"))
        Option(new PostalCode(postalCode))
      else
        None
    }
  }

  //TODO: PostalCodeCollection should have private attributes, fix createPackage so it doesn't directly access the postalCodes attribute
  case class Package private (weight: Double, postalCode: PostalCode)
  object Package {
    def createPackage(weight: Double, postalCode: PostalCode, validPostalCodeCollection: PostalCodeCollection): Option[Package] = {
      if (weight > 0 && validPostalCodeCollection.postalCodes.contains(postalCode))
        Option(new Package(weight, postalCode))
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
    
    //TODO: Create Person class, traits and companion object
  }






