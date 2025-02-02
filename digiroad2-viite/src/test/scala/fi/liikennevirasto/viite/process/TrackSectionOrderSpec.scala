package fi.liikennevirasto.viite.process

import fi.liikennevirasto.digiroad2.Point
import fi.liikennevirasto.digiroad2.asset.SideCode.{AgainstDigitizing, TowardsDigitizing}
import fi.liikennevirasto.digiroad2.asset.{AdministrativeClass, LinkGeomSource, SideCode}
import fi.liikennevirasto.digiroad2.postgis.PostGISDatabase
import fi.liikennevirasto.digiroad2.util.Track
import fi.liikennevirasto.viite.Dummies
import fi.liikennevirasto.viite.Dummies._
import fi.liikennevirasto.viite.dao.Discontinuity.{Continuous, Discontinuous}
import fi.liikennevirasto.viite.dao.TerminationCode.NoTermination
import fi.liikennevirasto.viite.dao._
import fi.liikennevirasto.viite.util.toProjectLink
import org.joda.time.DateTime
import org.scalatest.{FunSuite, Matchers}
import slick.driver.JdbcDriver.backend.Database
import slick.driver.JdbcDriver.backend.Database.dynamicSession

class TrackSectionOrderSpec extends FunSuite with Matchers {

  private def runWithRollback(f: => Unit): Unit = {
    Database.forDataSource(PostGISDatabase.ds).withDynTransaction {
      f
      dynamicSession.rollback()
    }
  }

  private def toDummyProjectLink(id: Long, geom: Seq[Point], track: Track = Track.Combined) = {
    dummyProjectLink(1L, 1L, track, Discontinuity.Continuous, 0, 10, Some(DateTime.now), linkId = id, status = LinkStatus.NotHandled, geometry = geom)
  }

  val projectId = 1
  val rap = Project(projectId, ProjectState.apply(1), "TestProject", "TestUser", DateTime.parse("2700-01-01"),
    "TestUser", DateTime.parse("1972-03-03"), DateTime.parse("2700-01-01"), "Some additional info",
    List.empty[ProjectReservedPart], Seq(), None)

  private def generateProjectLink(id: Long, geometry: Seq[Point], track: Track = Track.Combined) = {
    //TODO the road address now have the linear location id and as been setted to 1L
    toProjectLink(rap, LinkStatus.New)(RoadAddress(id, 1L, 5, 1, AdministrativeClass.Unknown, track, Continuous, 0L, 0L, Some(DateTime.parse("1901-01-01")), Some(DateTime.parse("1902-01-01")), Option("tester"), id, 0.0, 0.0, SideCode.TowardsDigitizing, 0, (None, None), geometry, LinkGeomSource.NormalLinkInterface, 8, NoTermination, 0))
  }

  test("Test orderProjectLinksTopologyByGeometry When is not dependent on the links order Then the links should be ordered") {
    val projectLink0 = dummyProjectLink(5, 1, Track.Combined, Continuous, 0L, 0L, Some(DateTime.parse("1901-01-01")), Some(DateTime.parse("1902-01-01")), 12345, 0.0, 0.0, SideCode.TowardsDigitizing, LinkStatus.New, geometry = Seq(Point(20.0, 10.0), Point(28, 15)))
    val projectLink1 = dummyProjectLink(5, 1, Track.Combined, Continuous, 0L, 0L, Some(DateTime.parse("1901-01-01")), Some(DateTime.parse("1902-01-01")), 12346, 0.0, 0.0, SideCode.AgainstDigitizing, LinkStatus.New, geometry = Seq(Point(42, 14),Point(28, 15)))
    val projectLink2 = dummyProjectLink(5, 1, Track.Combined, Continuous, 0L, 0L, Some(DateTime.parse("1901-01-01")), Some(DateTime.parse("1902-01-01")), 12347, 0.0, 0.0, SideCode.TowardsDigitizing, LinkStatus.New, geometry = Seq(Point(42, 14), Point(75, 19.2)))
    val projectLink3 = dummyProjectLink(5, 1, Track.Combined, Continuous, 0L, 0L, Some(DateTime.parse("1901-01-01")), Some(DateTime.parse("1902-01-01")), 12348, 0.0, 0.0, SideCode.AgainstDigitizing, LinkStatus.New, geometry = Seq(Point(103.0, 15.0),Point(75, 19.2)))
    val list = List(projectLink0, projectLink1, projectLink2, projectLink3)
    val (ordered, _) = TrackSectionOrder.orderProjectLinksTopologyByGeometry((Point(20.0, 10.0), Point(20.0, 10.0)), list)
    // Test that the result is not dependent on the order of the links
    list.permutations.foreach(l => {
      TrackSectionOrder.orderProjectLinksTopologyByGeometry((Point(20.0, 10.0), Point(20.0, 10.0)), l)._1 should be(ordered)
    })
  }

  test("Test orderProjectLinksTopologyByGeometry When combined track with one ill-fitting link direction after discontinuity Then links should be on right order") {
    val points = Seq(Seq(Point(100,110), Point(75, 130), Point(50,159)),
      Seq(Point(50,160), Point(0, 110), Point(0,60)),
      Seq(Point(0,60), Point(-50, 80), Point(-100, 110)),
      Seq(Point(-100,110), Point(-120, 140), Point(-150,210)))
    val geom = points.map(g =>
      if (g.head.y > g.last.y)
        g.reverse
      else g
    )
    val list = geom.zip(0 to 3).map{ case (g, id) =>
      dummyProjectLink(5, 1, Track.Combined, Continuous, 0L, 0L, Some(DateTime.parse("1901-01-01")), Some(DateTime.parse("1902-01-01")), id, 0.0, 0.0, SideCode.Unknown, LinkStatus.New, geometry = g)
    }
    runWithRollback {
      val (ordered, _) = TrackSectionOrder.orderProjectLinksTopologyByGeometry((Point(100, 110), Point(100, 110)), list)
      ordered.map(_.linkId) should be(Seq(0L, 1L, 2L, 3L))
    }
  }

  test("Test mValueRoundabout When roundabout with Towards facing starting link Then side code should be different") {
    val points = Seq(Seq(Point(150.00, 110.00),Point(146.19, 129.13),Point(135.36, 145.36)),
      Seq(Point(135.36, 145.36),Point(119.13, 156.19),Point(100.00, 160.00)),
      Seq(Point(100.00, 160.00),Point(80.87, 156.19),Point(64.64, 145.36)),
      Seq(Point(64.64, 145.36),Point(53.81, 129.13),Point(50.00, 110.00)),
      Seq(Point(50.00, 110.00),Point(53.81, 90.87),Point(64.64, 74.64)),
      Seq(Point(64.64, 74.64),Point(80.87, 63.81),Point(100.00, 60.00)),
      Seq(Point(100.00, 60.00),Point(119.13, 63.81),Point(135.36, 74.64)),
      Seq(Point(135.36, 74.64),Point(146.19, 90.87),Point(150.00, 110.00)))
    val geom = points.map(g =>
      if (g.head.y > g.last.y)
        g.reverse
      else g
    )
    val list = geom.zip(0 to 7).map{ case (g, id) =>
      dummyProjectLink(5, 1, Track.Combined, Continuous, 0L, 0L, Some(DateTime.parse("1901-01-01")), Some(DateTime.parse("1902-01-01")), id, 0.0, 0.0, SideCode.Unknown, LinkStatus.New, geometry = g)
    }
    TrackSectionOrder.isRoundabout(list) should be (true)
    TrackSectionOrder.isRoundabout(list.init) should be (false)
    TrackSectionOrder.isRoundabout(list.tail) should be (false)
    val result = TrackSectionOrder.mValueRoundabout(list)
    result should have size 8
    result.head.sideCode should be (TowardsDigitizing)
    result.forall(_.sideCode == result.head.sideCode) should be (false)
    result.head.geometry should be (list.head.geometry)
  }

  test("Test isRoundabout When invalid roundabout geometry Then throws exception") {
    val points = Seq(Seq(Point(150.00, 110.00),Point(146.19, 129.13),Point(135.36, 145.36)),
      Seq(Point(135.36, 145.36),Point(119.13, 156.19),Point(100.00, 160.00)),
      Seq(Point(100.00, 160.00),Point(80.87, 156.19),Point(80, 140)),
      Seq(Point(80, 140), Point(90, 130),Point(70, 100)),
      Seq(Point(70, 100),Point(60.00, 120.00),Point(50.00, 110.00)),
      Seq(Point(50.00, 110.00),Point(60.00, 83.81),Point(100.00, 60.00)),
      Seq(Point(100.00, 60.00),Point(119.13, 63.81),Point(135.36, 74.64)),
      Seq(Point(135.36, 74.64),Point(146.19, 90.87),Point(150.00, 110.00)))
    val geom = points.map(g =>
      if (g.head.y > g.last.y)
        g.reverse
      else g
    )
    val list = geom.zip(0 to 7).map{ case (g, id) =>
      dummyProjectLink(5, 1, Track.Combined, Continuous, 0L, 0L, Some(DateTime.parse("1901-01-01")), Some(DateTime.parse("1902-01-01")), id, 0.0, 0.0, SideCode.Unknown, LinkStatus.New, geometry = g)
    }
    TrackSectionOrder.isRoundabout(list) should be (true)
    intercept[InvalidGeometryException] {
      TrackSectionOrder.mValueRoundabout(list)
    }
  }

  test("Test mValueRoundabout When roundabout with Against facing starting link Then side code should be different") {
    val points = Seq(Seq(Point(100.00, 160.00),Point(80.87, 156.19),Point(64.64, 145.36)),
      Seq(Point(64.64, 145.36),Point(53.81, 129.13),Point(50.00, 110.00)),
      Seq(Point(50.00, 110.00),Point(53.81, 90.87),Point(64.64, 74.64)),
      Seq(Point(64.64, 74.64),Point(80.87, 63.81),Point(100.00, 60.00)),
      Seq(Point(100.00, 60.00),Point(119.13, 63.81),Point(135.36, 74.64)),
      Seq(Point(135.36, 74.64),Point(146.19, 90.87),Point(150.00, 110.00)),
      Seq(Point(150.00, 110.00),Point(146.19, 129.13),Point(135.36, 145.36)),
      Seq(Point(135.36, 145.36),Point(119.13, 156.19),Point(100.00, 160.00))
    )
    val geom = points.map(g =>
      if (g.head.y > g.last.y)
        g.reverse
      else g
    )
    val list = geom.zip(0 to 7).map{ case (g, id) =>
      dummyProjectLink(5, 1, Track.Combined, Continuous, 0L, 0L, Some(DateTime.parse("1901-01-01")), Some(DateTime.parse("1902-01-01")), id, 0.0, 0.0, SideCode.Unknown, LinkStatus.New, geometry = g)
    }
    TrackSectionOrder.isRoundabout(list) should be (true)
    TrackSectionOrder.isRoundabout(list.init) should be (false)
    TrackSectionOrder.isRoundabout(list.tail) should be (false)
    val result = TrackSectionOrder.mValueRoundabout(list)
    result should have size 8
    result.head.sideCode should be (AgainstDigitizing)
    result.forall(_.sideCode == result.head.sideCode) should be (false)
    result.head.geometry should be (list.head.geometry)
  }

  test("Test isRoundabout When ramp doesn't pass as a roundabout Then it is not a roundabout") {
    val points = Seq(Seq(Point(150.00, 40.00),Point(100.00, 160.00),Point(80.87, 156.19),Point(64.64, 145.36)),
      Seq(Point(64.64, 145.36),Point(53.81, 129.13),Point(50.00, 110.00)),
      Seq(Point(50.00, 110.00),Point(53.81, 90.87),Point(90.0, 74.64)),
      Seq(Point(90.0, 74.64), Point(160.00, 75.0))
    )
    val geom = points.map(g =>
      if (g.head.y > g.last.y)
        g.reverse
      else g
    )
    val list = geom.zip(0 to 7).map{ case (g, id) =>
      dummyProjectLink(5, 1, Track.Combined, Continuous, 0L, 0L, Some(DateTime.parse("1901-01-01")), Some(DateTime.parse("1902-01-01")), id, 0.0, 0.0, SideCode.Unknown, LinkStatus.New, geometry = g)
    }
    list.permutations.forall(l => !TrackSectionOrder.isRoundabout(l)) should be (true)
  }

  test("Test orderProjectLinksTopologyByGeometry When choosing the project link Then it should pick the most in right") {
    //                                 (25,15)
    //                                  / |
    //                                /   |
    //                              /     |
    //                            2L      2L
    //                           /        |
    //                         /          |
    //   |---------0L---------|-----1L----|
    //(10,10)            (20,10)       (30,10)
    val projectLinks = List(
      toDummyProjectLink(1L, Seq(Point(20, 10), Point(30, 10))),
      toDummyProjectLink(0L, Seq(Point(10, 10), Point(15, 10), Point(20, 10))),
      toDummyProjectLink(2L, Seq(Point(20, 10), Point(25, 15), Point(30, 10)))
    )
    runWithRollback {
      val (ordered, _) = TrackSectionOrder.orderProjectLinksTopologyByGeometry((Point(10, 10), Point(10, 10)), projectLinks)

      ordered.map(_.linkId) should be(List(0L, 1L, 2L))
    }
  }

  test("Test orderProjectLinksTopologyByGeometry When choosing two connected links Then pick the most forward one") {
    //                                            3L
    //                                   /|------------------|
    //                              2L /  |
    //                               /    |
    //                              |     | 4L
    //                              |\    |
    //                              |  \  |
    //                              | 7L \|-------------------|
    //                          1L  |     -         5L
    //                              |     |
    //                              |     | 6L
    //                              |     |
    //                              -     -
    //

    val projectLinks = List(
      toDummyProjectLink(1L, Seq(Point(2, 1), Point(2, 3), Point(2, 6)), Track.LeftSide),
      toDummyProjectLink(2L, Seq(Point(2, 6), Point(3, 7), Point(4, 8)), Track.LeftSide),
      toDummyProjectLink(3L, Seq(Point(4, 8), Point(6, 8), Point(8, 8)), Track.LeftSide),
      toDummyProjectLink(4L, Seq(Point(4, 4), Point(4, 6), Point(4, 8)), Track.RightSide),
      toDummyProjectLink(5L, Seq(Point(4, 4), Point(6, 4), Point(8, 4)), Track.RightSide),
      toDummyProjectLink(6L, Seq(Point(4, 1), Point(4, 2), Point(4, 4)), Track.RightSide),
      toDummyProjectLink(7L, Seq(Point(2, 6), Point(3, 5), Point(4, 4)), Track.RightSide)
    )
    runWithRollback {
      val (rightOrdered, leftOrdered) = TrackSectionOrder.orderProjectLinksTopologyByGeometry((Point(4, 1), Point(2, 1)), projectLinks)

      rightOrdered.map(_.linkId) should be(List(6L, 4L, 7L, 5L))
      leftOrdered.map(_.linkId) should be(List(1L, 2L, 3L))
    }
  }

  test("Test orderProjectLinksTopologyByGeometry When choosing the once connected Then there is any with the same track code on ordered list") {
    //                                 3l         4L
    //                             |--------|-----------|
    //                             |        |
    //                          1L |        | 2l
    //                             |        |
    //                             |        |
    //                             -        -

    val projectLinks = List(
      toDummyProjectLink(1L, Seq(Point(1, 1), Point(1, 2), Point(1, 4)), Track.LeftSide),
      toDummyProjectLink(2L, Seq(Point(3, 1), Point(3, 2), Point(3, 4)), Track.RightSide),
      toDummyProjectLink(3L, Seq(Point(1, 4), Point(2, 4), Point(3, 4)), Track.Combined),
      toDummyProjectLink(4L, Seq(Point(3, 4), Point(4, 4), Point(50, 4)), Track.Combined)
    )
    runWithRollback {
      val (rightOrdered, leftOrdered) = TrackSectionOrder.orderProjectLinksTopologyByGeometry((Point(3, 1), Point(1, 1)), projectLinks)

      rightOrdered.map(_.linkId) should be(List(2L, 3L, 4L))
      leftOrdered.map(_.linkId) should be(List(1L, 3L, 4L))
    }
  }

  test("Test orderProjectLinksTopologyByGeometry " +
                 "When two track road connects to one track with 90 deg angle from left to up" +
                 "Then both tracks should have correct link order.") {
    //                             |
    //                             │5L
    //                             │
    //                      2L     │4L
    //                             │
    //                 |-----------┥
    //                      1L     │3L
    //                 |-----------┘
    val connect1to3     = Point(328667.277, 6819926.413)
    val connect2to3and4 = Point(328667.496, 6819939.813)

    val startPointRight = Point(328387.331, 6819997.255)
    val startPointLeft  = Point(328390.798, 6820012.032)

    val connect4to5     = Point(328667.566, 6819950.224)
    val endPoint        = Point(328668.4, 6820062.203)

    val projectLinks = List(
      toDummyProjectLink(1L, Seq(connect1to3, startPointRight), Track.RightSide).copy(sideCode = SideCode.AgainstDigitizing),
      toDummyProjectLink(2L, Seq(connect2to3and4, startPointLeft), Track.LeftSide).copy(sideCode = SideCode.AgainstDigitizing),
      toDummyProjectLink(3L, Seq(connect1to3, connect2to3and4), Track.Combined).copy(sideCode = SideCode.TowardsDigitizing),
      toDummyProjectLink(4L, Seq(connect2to3and4, connect4to5), Track.Combined).copy(sideCode = SideCode.TowardsDigitizing),
      toDummyProjectLink(5L, Seq(connect4to5, endPoint), Track.Combined).copy(sideCode = SideCode.TowardsDigitizing)
    )
    runWithRollback {
      val (rightOrdered, leftOrdered) = TrackSectionOrder.orderProjectLinksTopologyByGeometry((startPointRight, startPointLeft), projectLinks)

      rightOrdered.map(_.linkId) should be(List(1L, 3L, 4L, 5L))
      leftOrdered.map(_.linkId) should be(List(2L, 3L, 4L, 5L))
    }
  }

  test("Test orderProjectLinksTopologyByGeometry When choosing the same track when there is 2 connected Then links with only one with same track code should be picked") {
    //                             -        -
    //                             |        |
    //                          3L |        | 4l
    //                             |        |
    //                             |        |
    //                   |---------|--------|
    //                        1L        2L

    val projectLinks = List(
      toDummyProjectLink(1L, Seq(Point(1, 1), Point(2, 1), Point(3, 1)), Track.Combined),
      toDummyProjectLink(2L, Seq(Point(3, 1), Point(4, 1), Point(5, 1)), Track.Combined),
      toDummyProjectLink(3L, Seq(Point(3, 1), Point(3, 2), Point(3, 4)), Track.LeftSide),
      toDummyProjectLink(4L, Seq(Point(5, 1), Point(5, 3), Point(5, 4)), Track.RightSide)
    )
    runWithRollback {
      val (rightOrdered, leftOrdered) = TrackSectionOrder.orderProjectLinksTopologyByGeometry((Point(1, 1), Point(1, 1)), projectLinks)

      rightOrdered.map(_.linkId) should be(List(1L, 2L, 4L))
      leftOrdered.map(_.linkId) should be(List(1L, 2L, 3L))
    }
  }

  test("Test findChainEndpoints When there is a discontinuity in a project link chain Then should not get the once connected points of the discontinuity") {
    //
    //                 (1,1)       (3,1)       (4,1)    (6,1)      (8,1)    (11,1)   (16,1)     (18,1)   (20,1)
    //                   |-----------|-----------|        |----------|---------|        |----------|--------|
    //                        1L          2L                   3L         4L               5L        6L
    val projectLinks = List(
      generateProjectLink(1L, Seq(Point(1, 1), Point(2, 1), Point(3, 1)), Track.LeftSide),
      generateProjectLink(2L, Seq(Point(3, 1), Point(4, 1)), Track.LeftSide),
      generateProjectLink(3L, Seq(Point(6, 1), Point(7, 1), Point(8, 1)), Track.Combined),
      generateProjectLink(4L, Seq(Point(8, 1), Point(10, 1), Point(11, 1)), Track.Combined),
      generateProjectLink(5L, Seq(Point(16, 1), Point(17, 1), Point(18, 1)), Track.LeftSide),
      generateProjectLink(6L, Seq(Point(18, 1), Point(19, 1), Point(20, 1)), Track.LeftSide)
    )
    val endPoints = TrackSectionOrder.findChainEndpoints(projectLinks)
    endPoints.size should be (2)
    val startPoint = endPoints.get(Point(1,1))
    val endPoint = endPoints.get(Point(20,1))
    startPoint.head.id should be (1L)
    endPoint.head.id should be (6L)
  }

  test("Test findChainEndpoints When adding completely new links before discontinuity Then when finding links in edges their points should also be on the edges") {
    //
    //                 (1,1)       (3,1)       (4,1)    (6,1)      (8,1)    (11,1)   (16,1)     (18,1)   (20,1)
    //                   |-----------|-----------|        |----------|---------|        |----------|--------|
    //                                                                                             | 7L     |8L
    //                                                                                             v        v
    //                                                                                           (18,0)  (20,0)
    //                        1L          2L                   3L         4L               5L        6L
    val projectLinks = List(
      generateProjectLink(1L, Seq(Point(1, 1), Point(2, 1), Point(3, 1)), Track.Combined),
      generateProjectLink(2L, Seq(Point(3, 1), Point(4, 1)), Track.Combined),
      generateProjectLink(3L, Seq(Point(6, 1), Point(7, 1), Point(8, 1)), Track.Combined).copy(startAddrMValue = 0L, endAddrMValue = 2L),
      generateProjectLink(4L, Seq(Point(8, 1), Point(10, 1), Point(11, 1)), Track.Combined).copy(startAddrMValue = 2L, endAddrMValue = 5L),
      generateProjectLink(5L, Seq(Point(16, 1), Point(17, 1), Point(18, 1)), Track.Combined).copy(startAddrMValue = 5L, endAddrMValue = 7L),
      generateProjectLink(6L, Seq(Point(18, 1), Point(19, 1), Point(20, 1)), Track.Combined).copy(startAddrMValue = 7L, endAddrMValue = 9L),
      generateProjectLink(7L, Seq(Point(18, 1), Point(18, 0)), Track.RightSide).copy(startAddrMValue = 9L, endAddrMValue = 10L),
      generateProjectLink(8L, Seq(Point(20, 1), Point(20, 0)), Track.LeftSide).copy(startAddrMValue = 9L, endAddrMValue = 10L)
    )
    val endPoints = TrackSectionOrder.findChainEndpoints(projectLinks)
    endPoints.size should be (2)
    val (chainStartPointLink, chainEndPointLink) = (endPoints.head, endPoints.last)
    chainStartPointLink._1 should be (chainStartPointLink._2.startingPoint)
    chainEndPointLink._1 should be (chainEndPointLink._2.endPoint)
  }

  test("Test orderProjectLinksTopologyByGeometry " +
                 "When a part of a road is transferred to another " +
                 "Then road sidecodes should be equal and the transferred part with opposite sidecode should be reversed.") {
    /* Two links on an unchanging roadway (0), and third that is on a changing roadway (1) to be reversed, and side code to be changed at ordering. */
    val points1      = Seq(Point(0, 0), Point(10, 0))
    val points2      = Seq(Point(10, 0), Point(20, 0))
    val points3      = Seq(Point(20, 0), Point(30, 0))
    val (rwn1, rwn2) = (99998, 99999)

    /* Two unchanged projectLinks and one with different side code and road istransferred to the unchanged road. */
    val projectLinks = List(
      Dummies.dummyProjectLink(1, 1, Track.Combined, Continuous, 0, 10, None, None, 0, 0, 10, SideCode.TowardsDigitizing, LinkStatus.UnChanged, geometry = points1, roadwayNumber = rwn1).copy(roadwayId = 0),
      Dummies.dummyProjectLink(1, 1, Track.Combined, Continuous, 10, 20, None, None, 0, 0, 10, SideCode.TowardsDigitizing, LinkStatus.UnChanged, geometry = points2, roadwayNumber = rwn1).copy(roadwayId = 0),
      Dummies.dummyProjectLink(1, 1, Track.Combined, Discontinuous, 20, 30, None, None, 1, 0, 10, SideCode.AgainstDigitizing, LinkStatus.Transfer, geometry = points3, roadwayNumber = rwn2).copy(roadwayId = 1)
    )

    val rws         = Seq(
      Roadway(0, rwn1, 1, 1, AdministrativeClass.State, Track.Combined, Discontinuity.Discontinuous, 0, 20, reversed = false, DateTime.parse("2020-01-03"), None, "test", None, 8L, NoTermination),
      Roadway(1, rwn2, 2, 1, AdministrativeClass.State, Track.Combined, Discontinuity.Discontinuous, 0, 10, reversed = false, DateTime.parse("2020-01-03"), None, "test", None, 8L, NoTermination)
    )

    runWithRollback {
      val roadwayDAO = new RoadwayDAO
      roadwayDAO.create(rws)

      /* Ordering given projectLinks between two points. The last link should get reversed status, and side code changed accordingly. */
      val (rightOrdered, _) = TrackSectionOrder.orderProjectLinksTopologyByGeometry((Point(0, 0), Point(30, 0)), projectLinks)

      rightOrdered.map(_.sideCode).foreach(sidecode => {
        sidecode should be(SideCode.TowardsDigitizing)
      })

      rightOrdered.map(_.reversed) should be(List(false, false, true))
    }
  }

}
