package fi.liikennevirasto.digiroad2.dao

import org.scalatest.{FunSuite, Matchers, Tag}

class PostGISMunicipalityProviderSpec extends FunSuite with Matchers {
  val provider = new PostGISMunicipalityProvider
  val elyCodeForLappi = Set(1)
  val elyCodeForAhvenanmaa = Set(0)
  val elyCodesForLappiAndAhvenanmaa = elyCodeForLappi ++ elyCodeForAhvenanmaa
  val municipalitiesForLappiEly = Set(47, 148, 240, 241, 261, 273, 320, 498, 583, 614, 683, 698, 732, 742, 751, 758, 845, 851, 854, 890, 976)
  val municipalitiesForAahvenanmaaEly = Set(35, 43, 60, 62, 65, 76, 170, 295, 318, 417, 438, 478, 736, 766, 771, 941)

  test("Test PostGISMunicipalityProvider.getMunicipalities() When searching for a municipalities with 1 ElyCodes Then return a set of all the municipality Ids for that Ely code.", Tag("db")) {
    val municipalities = provider.getMunicipalities(elyCodeForAhvenanmaa)
    municipalities shouldBe 'nonEmpty
    municipalities.toSet should equal (municipalitiesForAahvenanmaaEly)
  }

  test("Test PostGISMunicipalityProvider.getMunicipalities() When searching for a municipalities with 2 ElyCodes return a set of all the municipality Ids for both Ely codes.", Tag("db")) {
    val municipalities = provider.getMunicipalities(elyCodesForLappiAndAhvenanmaa)
    municipalities shouldBe 'nonEmpty
    municipalities.toSet should equal (municipalitiesForLappiEly ++ municipalitiesForAahvenanmaaEly)
  }
}
