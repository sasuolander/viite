package fi.liikennevirasto.digiroad2

import fi.liikennevirasto.digiroad2.GeometryUtils
import fi.liikennevirasto.digiroad2.asset.ConstructionType.InUse
import fi.liikennevirasto.digiroad2.asset.LinkGeomSource.NormalLinkInterface
import fi.liikennevirasto.digiroad2.asset._
import fi.liikennevirasto.viite.dao.{CalibrationPoint, RoadName}
import fi.liikennevirasto.viite.model.{Anomaly, RoadAddressLink}
import fi.liikennevirasto.viite.{RoadAddressService, RoadNameService}
import fi.liikennevirasto.digiroad2.asset.AdministrativeClass
import org.apache.commons.codec.binary.Base64
import org.joda.time.DateTime
import org.json4s.{DefaultFormats, Formats}
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{BeforeAndAfter, FunSuite, Tag}
import org.scalatra.test.scalatest.ScalatraSuite


class IntegrationApiSpec extends FunSuite with ScalatraSuite with BeforeAndAfter {
  protected implicit val jsonFormats: Formats = DefaultFormats

  val mockRoadAddressService: RoadAddressService = MockitoSugar.mock[RoadAddressService]
  when(mockRoadAddressService.getAllByMunicipality(235, None)).thenReturn(Seq())

  val mockRoadNameService: RoadNameService = MockitoSugar.mock[RoadNameService]

  private val integrationApi = new IntegrationApi(mockRoadAddressService, mockRoadNameService, new ViiteSwagger)
  addServlet(integrationApi, "/*")



  test("Test When asking for the road addresses by municipality but 1st not defining it and 2nd with it Then return status code 400 for the 1st and status code 200 for the 2nd.") {
    get("/road_address") {
      status should equal(400)
    }
    get("/road_address?municipality=235") {
      status should equal(200)
    }
  }

  test("Test integrationApi.roadAddressLinksToApi() When supliying a simple Road Address Link Then return a Sequence of a String to Value mappings that represent the Road Address Link.") {
    val geometry = Seq(Point(0.0, 0.0), Point(1.0, 0.0, 0.5), Point(4.0, 4.0, 1.5))
    // This roadAddressLink has linearLocationId equal to zero, just to compile.
    val roadAdressLink = RoadAddressLink(63298, 0, 5171208, geometry, GeometryUtils.geometryLength(geometry), AdministrativeClass.Municipality, UnknownLinkType, InUse, NormalLinkInterface, AdministrativeClass.Municipality, Some("Vt5"), None, BigInt(0), "", None, None, Map("linkId" -> 5171208, "segmentId" -> 63298), 5, 205, 1, 0, 0, 0, 6, "2015-01-01", "2015-12-31", 0.0, 0.0, SideCode.TowardsDigitizing, Some(CalibrationPoint(120, 1, 2)), None, Anomaly.None)
    integrationApi.roadAddressLinksToApi(Seq(roadAdressLink)) should be(Seq(Map(
      "muokattu_viimeksi" -> "",
      "geometryWKT" -> "LINESTRING ZM (0.000 0.000 0.000 0.000, 1.000 0.000 0.500 1.000, 4.000 4.000 1.500 6.000)",
      "id" -> 63298,
      "link_id" -> 5171208,
      "link_source" -> 1,
      "road_number" -> 5,
      "road_part_number" -> 205,
      "track_code" -> 1,
      "side_code" -> 2,
      "start_addr_m" -> 0,
      "end_addr_m" -> 6,
      "ely_code" -> 0,
      "road_type" -> 3,
      "administrative_class" -> 2,
      "discontinuity" -> 0,
      "start_date" -> "2015-01-01",
      "end_date" -> "2015-12-31",
      "calibration_points" -> Map("start" -> Some(Map("link_id" -> 120, "address_m_value" -> 2, "segment_m_value" -> 1.0)), "end" -> None)
    )))
  }

  test("Test integrationApi.roadAddressLinksToApi() for administrative class When AdministrativeClass is State.") {
    val geometry = Seq(Point(0.0, 0.0), Point(1.0, 0.0, 0.5), Point(4.0, 4.0, 1.5))
    // This roadAddressLink has linearLocationId equal to zero, just to compile.
    val roadAdressLink = RoadAddressLink(63298, 0, 5171208, geometry, GeometryUtils.geometryLength(geometry), AdministrativeClass.State, UnknownLinkType, InUse, NormalLinkInterface, AdministrativeClass.State, Some("Vt5"), None, BigInt(0), "", None, None, Map("linkId" -> 5171208, "segmentId" -> 63298), 5, 205, 1, 0, 0, 0, 6, "2015-01-01", "2015-12-31", 0.0, 0.0, SideCode.TowardsDigitizing, Some(CalibrationPoint(120, 1, 2)), None, Anomaly.None)
    integrationApi.roadAddressLinksToApi(Seq(roadAdressLink)).head("road_type") shouldBe  1
  }
  test("Test integrationApi.roadAddressLinksToApi() When for administrative class When AdministrativeClass is Municipality.") {
    val geometry = Seq(Point(0.0, 0.0), Point(1.0, 0.0, 0.5), Point(4.0, 4.0, 1.5))
    // This roadAddressLink has linearLocationId equal to zero, just to compile.
    val roadAdressLink = RoadAddressLink(63298, 0, 5171208, geometry, GeometryUtils.geometryLength(geometry), AdministrativeClass.Municipality, UnknownLinkType, InUse, NormalLinkInterface, AdministrativeClass.Municipality, Some("Vt5"), None, BigInt(0), "", None, None, Map("linkId" -> 5171208, "segmentId" -> 63298), 5, 205, 1, 0, 0, 0, 6, "2015-01-01", "2015-12-31", 0.0, 0.0, SideCode.TowardsDigitizing, Some(CalibrationPoint(120, 1, 2)), None, Anomaly.None)
    integrationApi.roadAddressLinksToApi(Seq(roadAdressLink)).head("road_type") shouldBe 3
  }
  test("Test integrationApi.roadAddressLinksToApi() When for administrative class When AdministrativeClass is Private.") {
    val geometry = Seq(Point(0.0, 0.0), Point(1.0, 0.0, 0.5), Point(4.0, 4.0, 1.5))
    // This roadAddressLink has linearLocationId equal to zero, just to compile.
    val roadAdressLink = RoadAddressLink(63298, 0, 5171208, geometry, GeometryUtils.geometryLength(geometry), AdministrativeClass.Private, UnknownLinkType, InUse, NormalLinkInterface, AdministrativeClass.Private, Some("Vt5"), None, BigInt(0), "", None, None, Map("linkId" -> 5171208, "segmentId" -> 63298), 5, 205, 1, 0, 0, 0, 6, "2015-01-01", "2015-12-31", 0.0, 0.0, SideCode.TowardsDigitizing, Some(CalibrationPoint(120, 1, 2)), None, Anomaly.None)
    integrationApi.roadAddressLinksToApi(Seq(roadAdressLink)).head("road_type") shouldBe 5
  }
  test("Test integrationApi.roadAddressLinksToApi() When for administrative class When AdministrativeClass is Unknown.") {
    val geometry = Seq(Point(0.0, 0.0), Point(1.0, 0.0, 0.5), Point(4.0, 4.0, 1.5))
    // This roadAddressLink has linearLocationId equal to zero, just to compile.
    val roadAdressLink = RoadAddressLink(63298, 0, 5171208, geometry, GeometryUtils.geometryLength(geometry), AdministrativeClass.Unknown, UnknownLinkType, InUse, NormalLinkInterface, AdministrativeClass.Unknown, Some("Vt5"), None, BigInt(0), "", None, None, Map("linkId" -> 5171208, "segmentId" -> 63298), 5, 205, 1, 0, 0, 0, 6, "2015-01-01", "2015-12-31", 0.0, 0.0, SideCode.TowardsDigitizing, Some(CalibrationPoint(120, 1, 2)), None, Anomaly.None)
    integrationApi.roadAddressLinksToApi(Seq(roadAdressLink)).head("road_type") shouldBe 99
  }

  test("Test integrationApi.geometryWKT() When using a 3D sequence of points and a start and end measures that fit in the geometry Then return a proper geometry in LineString format.") {
    val (header, returnTxt) =
      integrationApi.geometryWKT(Seq(Point(0.0, 0.0), Point(1.0, 0.0, 0.5), Point(4.0, 4.0, 1.5)), 0L, 6L)
    header should be ("geometryWKT")
    returnTxt should be ("LINESTRING ZM (0.000 0.000 0.000 0.000, 1.000 0.000 0.500 1.000, 4.000 4.000 1.500 6.000)")
  }

  def date(year: Int, month: Int, day: Int): Option[DateTime] = {
    Option(new DateTime(year, month, day, 0, 0, 0))
  }

  test("Test When asking for changes in the roadnames but missing the Since parameter Then returns status code 400") {
    get("/roadnames/changes") {
      status should equal(400)
    }
  }

  test("Test When asking for changes in the roadnames including the Since parameter but giving it no value Then returns status code 400") {
    get("/roadnames/changes?since=") {
      status should equal(400)
    }
  }

  test("Test When asking for changes in the roadnames including the Since parameter but giving it a incorrect value Then returns status code 400") {
    get("/roadnames/changes?since=abc") {
      status should equal(400)
    }
  }

  test("Test When asking for changes in the roadnames including the Since parameter but giving it a incorrect date Then returns status code 400") {
    get("/roadnames/changes?since=1.1.2018") {
      status should equal(400)
    }
  }

  test("Test When asking for changes in the roadnames correctly including the Since but with no data to return Then returns status code 200 and a empty array as the response body.") {
    when(mockRoadNameService.getUpdatedRoadNames(any[DateTime], any[Option[DateTime]])).thenReturn(Right(Seq()))
    get("/roadnames/changes?since=9999-01-01") {
      status should equal(200)
      response.getHeader("Content-Type").toLowerCase should equal("application/json;charset=utf-8")
      response.body.toString should equal("[]")
    }
  }

  test("Test When asking for changes in the roadnames correctly including the Since but with 1 update to 1 road Then returns status code 200 and a filled array with the info for the road.") {
    when(mockRoadNameService.getUpdatedRoadNames(any[DateTime], any[Option[DateTime]])).thenReturn(
      Right(Seq(
        RoadName(1, 2, "MYROAD", date(2018, 2, 2), None, date(2018, 1, 1), None, "MOCK")
      ))
    )
    get("/roadnames/changes?since=2018-01-01") {
      status should equal(200)
      response.body should equal(
        "[{\"road_number\":2,\"names\":[{\"created_by\":\"MOCK\",\"start_date\":\"" + DateTime.parse("2018-02-02").toString + "\",\"road_name\":\"MYROAD\",\"end_date\":null,\"change_date\":\"" + DateTime.parse("2018-01-01").toString + "\"}]}]"
      )
    }
  }

  test("Test When asking for changes in the roadnames correctly including the Since with many updates to return to 1 road Then returns status code 200 and a filled array with the info for the updates.") {
    when(mockRoadNameService.getUpdatedRoadNames(any[DateTime], any[Option[DateTime]])).thenReturn(
      Right(Seq(
        RoadName(3, 2, "MY ROAD", date(2018, 2, 2), None, date(2018, 1, 1), None, "MOCK"),
        RoadName(2, 2, "THEROAD", date(2000, 2, 2), date(2018, 2, 1), date(2018, 1, 1), None, "MOCK"),
        RoadName(1, 2, "OLDROAD", date(1900, 2, 2), date(2000, 2, 1), date(1900, 1, 1), None, "MOCK")
      ))
    )
    get("/roadnames/changes?since=2018-01-01") {
      status should equal(200)
      response.body should equal(
        "[{\"road_number\":2,\"names\":[" +
          "{\"created_by\":\"MOCK\",\"start_date\":\"" + DateTime.parse("2018-02-02").toString + "\",\"road_name\":\"MY ROAD\",\"end_date\":null,\"change_date\":\"" + DateTime.parse("2018-01-01").toString + "\"}," +
          "{\"created_by\":\"MOCK\",\"start_date\":\"" + DateTime.parse("2000-02-02").toString + "\",\"road_name\":\"THEROAD\",\"end_date\":\"" + DateTime.parse("2018-02-01").toString + "\",\"change_date\":\"" + DateTime.parse("2018-01-01").toString + "\"}," +
          "{\"created_by\":\"MOCK\",\"start_date\":\"" + DateTime.parse("1900-02-02").toString + "\",\"road_name\":\"OLDROAD\",\"end_date\":\"" + DateTime.parse("2000-02-01").toString + "\",\"change_date\":\"" + DateTime.parse("1900-01-01").toString + "\"}]}]"
      )
    }
  }

  test("Test When asking for changes in the roadnames correctly including the Since with many updates to return to 2 roads Then returns status code 200 and a filled array with the info for the updates.") {
    when(mockRoadNameService.getUpdatedRoadNames(any[DateTime], any[Option[DateTime]])).thenReturn(
      Right(Seq(
        RoadName(4, 3, "ANOTHER ROAD", date(2017, 12, 12), None, date(2017, 12, 1), None, "MOCK"),
        RoadName(3, 2, "MY ROAD", date(2018, 2, 2), None, date(2017, 12, 1), None, "MOCK"),
        RoadName(2, 2, "THEROAD", date(2000, 2, 2), date(2018, 2, 1), date(2017, 12, 1), None, "MOCK"),
        RoadName(1, 2, "OLDROAD", date(1900, 2, 2), date(2000, 2, 1), date(1900, 1, 1), None, "MOCK")
      ))
    )
    get("/roadnames/changes?since=2017-12-01") {
      status should equal(200)
      response.body should equal(
        "[" +
          "{\"road_number\":2,\"names\":[" +
          "{\"created_by\":\"MOCK\",\"start_date\":\"" + DateTime.parse("2018-02-02").toString + "\",\"road_name\":\"MY ROAD\",\"end_date\":null,\"change_date\":\"" + DateTime.parse("2017-12-01").toString + "\"}," +
          "{\"created_by\":\"MOCK\",\"start_date\":\"" + DateTime.parse("2000-02-02").toString + "\",\"road_name\":\"THEROAD\",\"end_date\":\"" + DateTime.parse("2018-02-01").toString + "\",\"change_date\":\"" + DateTime.parse("2017-12-01").toString + "\"}," +
          "{\"created_by\":\"MOCK\",\"start_date\":\"" + DateTime.parse("1900-02-02").toString + "\",\"road_name\":\"OLDROAD\",\"end_date\":\"" + DateTime.parse("2000-02-01").toString + "\",\"change_date\":\"" + DateTime.parse("1900-01-01").toString + "\"}" +
          "]}," +
          "{\"road_number\":3,\"names\":[" +
          "{\"created_by\":\"MOCK\",\"start_date\":\"" + DateTime.parse("2017-12-12").toString + "\",\"road_name\":\"ANOTHER ROAD\",\"end_date\":null,\"change_date\":\"" + DateTime.parse("2017-12-01").toString + "\"}" +
          "]}" +
          "]"
      )
    }
  }

  test("Test When asking for changes in the roadnames correctly including the Since with no updates to any road Then returns status code 200 and the returning array should be empty.") {
    when(mockRoadNameService.getUpdatedRoadNames(any[DateTime], any[Option[DateTime]])).thenReturn(Right(Seq()))
    get("/roadnames/changes?since=9999-01-01&until=9999-01-01") {
      status should equal(200)
      response.getHeader("Content-Type").toLowerCase should equal("application/json;charset=utf-8")
      response.body should equal("[]")
    }
  }

  test("Test When asking for changes in the roadnames correctly including the Since with 1 change to a single road Then returns status code 200 and the returning array should contain that road info.") {
    when(mockRoadNameService.getUpdatedRoadNames(any[DateTime], any[Option[DateTime]])).thenReturn(
      Right(Seq(
        RoadName(1, 2, "MYROAD", date(2018, 2, 2), None, date(2018, 1, 1), None, "MOCK")
      ))
    )
    get("/roadnames/changes?since=2018-01-01&until=2018-01-03") {
      status should equal(200)
      response.body should equal(
        "[{\"road_number\":2,\"names\":[{\"created_by\":\"MOCK\",\"start_date\":\"" + DateTime.parse("2018-02-02").toString + "\",\"road_name\":\"MYROAD\",\"end_date\":null,\"change_date\":\"" + DateTime.parse("2018-01-01").toString + "\"}]}]"
      )
    }
  }

  test("Test When asking for changes in the roadnames correctly including the Since with some changes to a single road Then returns status code 200 and the returning array should contain the multiple road info.") {
    when(mockRoadNameService.getUpdatedRoadNames(any[DateTime], any[Option[DateTime]])).thenReturn(
      Right(Seq(
        RoadName(3, 2, "MY ROAD", date(2018, 2, 2), None, date(2018, 1, 1), None, "MOCK"),
        RoadName(2, 2, "THEROAD", date(2000, 2, 2), date(2018, 2, 1), date(2018, 1, 1), None, "MOCK"),
        RoadName(1, 2, "OLDROAD", date(1900, 2, 2), date(2000, 2, 1), date(1900, 1, 1), None, "MOCK")
      ))
    )
    get("/roadnames/changes?since=2018-01-01&until=2018-01-03") {
      status should equal(200)
      response.body should equal(
        "[{\"road_number\":2,\"names\":[" +
          "{\"created_by\":\"MOCK\",\"start_date\":\"" + DateTime.parse("2018-02-02").toString + "\",\"road_name\":\"MY ROAD\",\"end_date\":null,\"change_date\":\"" + DateTime.parse("2018-01-01").toString + "\"}," +
          "{\"created_by\":\"MOCK\",\"start_date\":\"" + DateTime.parse("2000-02-02").toString + "\",\"road_name\":\"THEROAD\",\"end_date\":\"" + DateTime.parse("2018-02-01").toString + "\",\"change_date\":\"" + DateTime.parse("2018-01-01").toString + "\"}," +
          "{\"created_by\":\"MOCK\",\"start_date\":\"" + DateTime.parse("1900-02-02").toString + "\",\"road_name\":\"OLDROAD\",\"end_date\":\"" + DateTime.parse("2000-02-01").toString + "\",\"change_date\":\"" + DateTime.parse("1900-01-01").toString + "\"}]}]"
      )
    }
  }

  test("Test When asking for changes in the roadnames correctly including the Since with some changes to multiple roads Then returns status code 200 and the returning array should contain the multiple road info.") {
    when(mockRoadNameService.getUpdatedRoadNames(any[DateTime], any[Option[DateTime]])).thenReturn(
      Right(Seq(
        RoadName(4, 3, "ANOTHER ROAD", date(2017, 12, 12), None, date(2017, 12, 1), None, "MOCK"),
        RoadName(3, 2, "MY ROAD", date(2018, 2, 2), None, date(2017, 12, 1), None, "MOCK"),
        RoadName(2, 2, "THEROAD", date(2000, 2, 2), date(2018, 2, 1), date(2017, 12, 1), None, "MOCK"),
        RoadName(1, 2, "OLDROAD", date(1900, 2, 2), date(2000, 2, 1), date(1900, 1, 1), None, "MOCK")
      ))
    )
    get("/roadnames/changes?since=2017-12-01&until=2018-12-03") {
      status should equal(200)
      response.body should equal(
        "[" +
          "{\"road_number\":2,\"names\":[" +
          "{\"created_by\":\"MOCK\",\"start_date\":\"" + DateTime.parse("2018-02-02").toString + "\",\"road_name\":\"MY ROAD\",\"end_date\":null,\"change_date\":\"" + DateTime.parse("2017-12-01").toString + "\"}," +
          "{\"created_by\":\"MOCK\",\"start_date\":\"" + DateTime.parse("2000-02-02").toString + "\",\"road_name\":\"THEROAD\",\"end_date\":\"" + DateTime.parse("2018-02-01").toString + "\",\"change_date\":\"" + DateTime.parse("2017-12-01").toString + "\"}," +
          "{\"created_by\":\"MOCK\",\"start_date\":\"" + DateTime.parse("1900-02-02").toString + "\",\"road_name\":\"OLDROAD\",\"end_date\":\"" + DateTime.parse("2000-02-01").toString + "\",\"change_date\":\"" + DateTime.parse("1900-01-01").toString + "\"}" +
          "]}," +
          "{\"road_number\":3,\"names\":[" +
          "{\"created_by\":\"MOCK\",\"start_date\":\"" + DateTime.parse("2017-12-12").toString + "\",\"road_name\":\"ANOTHER ROAD\",\"end_date\":null,\"change_date\":\"" + DateTime.parse("2017-12-01").toString + "\"}" +
          "]}" +
          "]"
      )
    }
  }
}
