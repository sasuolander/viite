package fi.liikennevirasto.viite.dao

import fi.liikennevirasto.digiroad2.Point
import fi.liikennevirasto.digiroad2.asset.{BoundingRectangle, LinkGeomSource, SideCode}
import fi.liikennevirasto.digiroad2.dao.Sequences
import fi.liikennevirasto.digiroad2.oracle.OracleDatabase
import fi.liikennevirasto.viite.NewIdValue
import org.joda.time.DateTime
import org.scalatest.{FunSuite, Matchers}
import slick.driver.JdbcDriver.backend.Database
import slick.driver.JdbcDriver.backend.Database.dynamicSession

class JunctionPointDAOSpec extends FunSuite with Matchers {

  def runWithRollback(f: => Unit): Unit = {
    Database.forDataSource(OracleDatabase.ds).withDynTransaction {
      f
      dynamicSession.rollback()
    }
  }

  val dao = new JunctionPointDAO
  val junctionDAO = new JunctionDAO
  val roadwayPointDAO = new RoadwayPointDAO
  val linearLocationDAO = new LinearLocationDAO

  val testRoadwayPoint1 = RoadwayPoint(NewIdValue, -1, 10, "Test", None, None, None)

  val testJunctionPoint1 = JunctionPoint(NewIdValue, BeforeAfter.Before, -1, -1,
    DateTime.parse("2019-01-01"), None, DateTime.parse("2019-01-01"), None, None, None, -1, 10)
  val testJunctionPoint2 = JunctionPoint(NewIdValue, BeforeAfter.After, -1, -1,
    DateTime.parse("2019-01-01"), None, DateTime.parse("2019-01-01"), None, None, None, -1, 10)

  val testJunction1 = Junction(NewIdValue, -1, None, DateTime.parse("2019-01-01"), None,
    DateTime.parse("2019-01-01"), None, None, None)

  val testLinearLocation1 = LinearLocation(NewIdValue, 1, 1000l, 0.0, 2.8, SideCode.TowardsDigitizing, 10000000000l,
    (None, None), Seq(Point(99.0, 99.0), Point(101.0, 101.0)), LinkGeomSource.NormalLinkInterface, -1)


  test("Test create When nothing to create Then return empty Seq") {
    runWithRollback {
      val ids = dao.create(Seq())
      ids.isEmpty should be(true)
    }
  }
/*
  test("Test create When one created Then return Seq with one id") {
    runWithRollback {
      val roadwayPointId = roadwayPointDAO.create(testRoadwayPoint1.copy(roadwayNumber = Sequences.nextRoadwayNumber))
      val junctionId = junctionDAO.create(testJunction1)
      val ids = dao.create(Seq(testJunctionPoint1.copy(roadwayPointId = roadwayPointId)))
      ids.size should be(1)
    }
  }

  test("Test create When two created Then return Seq with two ids") {
    runWithRollback {
      val roadwayPointId1 = roadwayPointDAO.create(testRoadwayPoint1.copy(roadwayNumber = Sequences.nextRoadwayNumber))
      val ids = dao.create(Seq(testNodePoint1.copy(roadwayPointId = roadwayPointId1),
        testNodePoint2.copy(roadwayPointId = roadwayPointId1)))
      ids.size should be(2)
    }
  }

  test("Test fetchNodePointsByNodeId When non-existing nodeId Then return empty Seq") {
    runWithRollback {
      val roadwayPointId1 = roadwayPointDAO.create(testRoadwayPoint1.copy(roadwayNumber = Sequences.nextRoadwayNumber))
      val nodeId = nodeDAO.create(Seq(testJunction1)).head
      dao.create(Seq(testNodePoint1.copy(roadwayPointId = roadwayPointId1, nodeId = Some(nodeId)),
        testNodePoint2.copy(roadwayPointId = roadwayPointId1, nodeId = Some(nodeId))))
      val nodePoints = dao.fetchNodePointsByNodeId(Seq(-1))
      nodePoints.isEmpty should be(true)
    }
  }

  test("Test fetchNodePointsByNodeId When existing nodeId Then return node points") {
    runWithRollback {
      val roadwayPointId1 = roadwayPointDAO.create(testRoadwayPoint1.copy(roadwayNumber = Sequences.nextRoadwayNumber))
      val nodeId = nodeDAO.create(Seq(testJunction1)).head
      dao.create(Seq(testNodePoint1.copy(roadwayPointId = roadwayPointId1, nodeId = Some(nodeId)),
        testNodePoint2.copy(roadwayPointId = roadwayPointId1, nodeId = Some(nodeId))))
      val nodePoints = dao.fetchNodePointsByNodeId(Seq(nodeId))
      nodePoints.size should be(2)
      nodePoints.filter(n => n.nodeId == Some(nodeId)).size should be(2)
    }
  }

  test("Test fetchTemplatesByBoundingBox When no matches Then return empty Seq") {
    runWithRollback {
      val roadwayNumber = Sequences.nextRoadwayNumber
      val roadwayPointId1 = roadwayPointDAO.create(testRoadwayPoint1.copy(roadwayNumber = roadwayNumber))
      dao.create(Seq(testNodePoint1.copy(roadwayPointId = roadwayPointId1, nodeId = None),
        testNodePoint2.copy(roadwayPointId = roadwayPointId1, nodeId = None)))
      linearLocationDAO.create(Seq(testLinearLocation1.copy(roadwayNumber = roadwayNumber)))
      val nodePoints = dao.fetchTemplatesByBoundingBox(BoundingRectangle(Point(0, 0), Point(1, 1)))
      nodePoints.isEmpty should be(true)
    }
  }

  test("Test fetchTemplatesByBoundingBox When matches Then return node points") {
    runWithRollback {
      val roadwayNumber = Sequences.nextRoadwayNumber
      val roadwayPointId1 = roadwayPointDAO.create(testRoadwayPoint1.copy(roadwayNumber = roadwayNumber))
      dao.create(Seq(testNodePoint1.copy(roadwayPointId = roadwayPointId1, nodeId = None),
        testNodePoint2.copy(roadwayPointId = roadwayPointId1, nodeId = None)), "Test")
      linearLocationDAO.create(Seq(testLinearLocation1.copy(roadwayNumber = roadwayNumber)))
      val nodePoints = dao.fetchTemplatesByBoundingBox(BoundingRectangle(Point(98, 98), Point(102, 102)))
      nodePoints.size should be(2)
      nodePoints.filter(n => n.roadwayNumber == roadwayNumber).size should be(2)
      nodePoints.filter(n => n.addrM == testRoadwayPoint1.addrMValue).size should be(2)
      nodePoints.filter(n => n.createdBy == Some("Test")).size should be(2)
    }
  }
*/
}