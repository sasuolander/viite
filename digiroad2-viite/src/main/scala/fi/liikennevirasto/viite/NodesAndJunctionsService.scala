package fi.liikennevirasto.viite

import fi.liikennevirasto.digiroad2.asset.BoundingRectangle
import fi.liikennevirasto.digiroad2.dao.Sequences
import fi.liikennevirasto.digiroad2.oracle.OracleDatabase
import fi.liikennevirasto.digiroad2.util.LogUtils.time
import fi.liikennevirasto.viite.dao._
import fi.liikennevirasto.viite.process.RoadwayAddressMapper
import org.joda.time.DateTime
import org.slf4j.LoggerFactory

import scala.util.control.NonFatal

class NodesAndJunctionsService(roadwayDAO: RoadwayDAO, roadwayPointDAO: RoadwayPointDAO, linearLocationDAO: LinearLocationDAO, nodeDAO: NodeDAO, nodePointDAO: NodePointDAO, junctionDAO: JunctionDAO, junctionPointDAO: JunctionPointDAO) {
  case class CompleteNode(node: Option[Node], nodePoints: Seq[NodePoint], junctions: Map[Junction, Seq[JunctionPoint]])

  def withDynTransaction[T](f: => T): T = OracleDatabase.withDynTransaction(f)

  def withDynSession[T](f: => T): T = OracleDatabase.withDynSession(f)

  private val logger = LoggerFactory.getLogger(getClass)

  val roadwayAddressMapper = new RoadwayAddressMapper(roadwayDAO, linearLocationDAO)

  def getNodesByRoadAttributes(roadNumber: Long, minRoadPartNumber: Option[Long], maxRoadPartNumber: Option[Long]): Either[String, Seq[(Node, RoadAttributes)]] = {
    withDynSession {
      try {
        // if the result set has more than 50 rows but the road attributes can't be narrowed down, it shows the results anyway
        nodeDAO.fetchByRoadAttributes(roadNumber, minRoadPartNumber, maxRoadPartNumber) match {
          case nodes
            if nodes.size <= MaxAllowedNodes ||
              minRoadPartNumber.isDefined && maxRoadPartNumber.isDefined && minRoadPartNumber.get == maxRoadPartNumber.get ||
              minRoadPartNumber.isDefined && maxRoadPartNumber.isEmpty || minRoadPartNumber.isEmpty && maxRoadPartNumber.isDefined => Right(nodes)
          case _ => Left(ReturnedTooManyNodesErrorMessage)
        }
      } catch {
        case e if NonFatal(e) => {
          logger.error("Failed to fetch nodes.", e)
          Left(e.getMessage)
        }
      }
    }
  }

  def getNodesByBoundingBox(boundingRectangle: BoundingRectangle): Seq[Node] = {
    withDynSession{
      time(logger, "Fetch nodes with junctions") {
        nodeDAO.fetchByBoundingBox(boundingRectangle)
      }
    }
  }

  def getNodesWithJunctionByBoundingBox(boundingRectangle: BoundingRectangle): Map[Option[Node], (Seq[NodePoint], Map[Junction, Seq[JunctionPoint]])] = {
    withDynSession {
      time(logger, "Fetch nodes with junctions") {
        val nodes = nodeDAO.fetchByBoundingBox(boundingRectangle)
        val nodePoints = nodePointDAO.fetchNodePointsByNodeId(nodes.map(_.id))
        val junctions = junctionDAO.fetchJunctionByNodeIds(nodes.map(_.id))
        val junctionPoints = junctionPointDAO.fetchJunctionPointsByJunctionIds(junctions.map(_.id))
        val nodesAndJunctions = nodes.map {
          node =>
            (Option(node),
              (
                nodePoints.filter(np => np.nodeId.isDefined && np.nodeId.get == node.id),
                junctions.filter(j => j.nodeId.isDefined && j.nodeId.get == node.id).map {
                  junction =>
                    (
                      junction, junctionPoints.filter(_.junctionId == junction.id)
                    )
                }.toMap
              )
            )
        } ++ Seq((None, getTemplatesByBoundingBox(boundingRectangle)))
        nodesAndJunctions.toMap
      }
    }
  }

  def getTemplatesByBoundingBox(boundingRectangle: BoundingRectangle): (Seq[NodePoint], Map[Junction, Seq[JunctionPoint]]) = {
    withDynSession {
      time(logger, "Fetch NodePoint and Junction + JunctionPoint templates") {
        val junctionPoints = junctionPointDAO.fetchTemplatesByBoundingBox(boundingRectangle)
        val junctions = junctionDAO.fetchByIds(junctionPoints.map(_.junctionId))
        val nodePoints = nodePointDAO.fetchTemplatesByBoundingBox(boundingRectangle)
        (nodePoints, junctions.map {junction => (junction, junctionPoints.filter(_.junctionId == junction.id))}.toMap)
      }
    }
  }

  def handleJunctionPointTemplates(projectLinks: Seq[ProjectLink]): Unit = {
    val filteredLinks = projectLinks.filter(pl => RoadClass.nodeAndJunctionRoadClass.flatMap(_.roads).contains(pl.roadNumber.toInt))
    filteredLinks.foreach{ link =>
      val roadNumberLimits = Seq((0, 19999), (40001, 69999))
      val roadsInHead = roadwayAddressMapper.getRoadAddressesByBoundingBox(BoundingRectangle(link.getFirstPoint, link.getFirstPoint), roadNumberLimits).filterNot(rw => rw.roadNumber == link.roadNumber && rw.roadPartNumber == link.roadPartNumber).filter(_.connected(link.getFirstPoint))
      val roadsOutTail = roadwayAddressMapper.getRoadAddressesByBoundingBox(BoundingRectangle(link.getLastPoint, link.getLastPoint), roadNumberLimits).filterNot(rw => rw.roadNumber == link.roadNumber && rw.roadPartNumber == link.roadPartNumber).filter(ra => link.connected(ra.getFirstPoint))

      //check existance of junction points connecting to head point of project link
      roadsInHead.foreach { r =>
        if (junctionPointDAO.fetchJunctionPointsByRoadwayPoints(r.roadwayNumber, r.endAddrMValue).isEmpty)
          junctionPointDAO.create(Seq(JunctionPoint(NewIdValue, BeforeAfter.After, Sequences.nextRoadwayPointId, 0L, DateTime.now, None, DateTime.now, None, link.createdBy, Some(DateTime.now), r.roadwayNumber, r.endAddrMValue)))

        if (junctionPointDAO.fetchJunctionPointsByRoadwayPoints(link.roadwayNumber, link.startAddrMValue).isEmpty)
          junctionPointDAO.create(Seq(JunctionPoint(NewIdValue, BeforeAfter.Before, Sequences.nextRoadwayPointId, 0L, DateTime.now, None, DateTime.now, None, link.createdBy, Some(DateTime.now), link.roadwayNumber, link.startAddrMValue)))
      }

      //check existing of junction points connecting to head point of project link
      roadsOutTail.foreach { r =>
        if (junctionPointDAO.fetchJunctionPointsByRoadwayPoints(r.roadwayNumber, r.endAddrMValue).isEmpty)
          junctionPointDAO.create(Seq(JunctionPoint(NewIdValue, BeforeAfter.Before, Sequences.nextRoadwayPointId, 0L, DateTime.now, None, DateTime.now, None, link.createdBy, Some(DateTime.now), r.roadwayNumber, r.startAddrMValue)))

        if (junctionPointDAO.fetchJunctionPointsByRoadwayPoints(link.roadwayNumber, link.startAddrMValue).isEmpty)
          junctionPointDAO.create(Seq(JunctionPoint(NewIdValue, BeforeAfter.After, Sequences.nextRoadwayPointId, 0L, DateTime.now, None, DateTime.now, None, link.createdBy, Some(DateTime.now), link.roadwayNumber, link.endAddrMValue)))
      }
    }
  }

  def removeObsoleteJunctions(projectLinks: Seq[ProjectLink], username: String = "-"): Unit = {
    val endDate = projectLinks.head.endDate
    val terminatedLinks = projectLinks.filter(pl => pl.endDate.isDefined)
    val terminatedRoadwayNumbers = terminatedLinks.map(_.roadwayNumber).distinct
    val currentRoadwayPoints = roadwayPointDAO.fetchByRoadwayNumbers(terminatedRoadwayNumbers)
    val obsoleteJunctionPoints = junctionPointDAO.fetchByRoadwayPointIds(currentRoadwayPoints.map(_.id))

    // Expire current junction point rows
    junctionPointDAO.expireById(obsoleteJunctionPoints.map(_.id))

    // Remove junctions without junction points
    val obsoleteJunctions = junctionDAO.fetchWithoutJunctionPointsById(obsoleteJunctionPoints.map(_.junctionId).distinct)
    junctionDAO.expireById(obsoleteJunctions.map(_.id))

    // Handle obsolete junction points of valid and obsolete junctions separately
    val (obsoleteJunctionPointsOfValidJunctions, obsoleteJunctionPointsOfObsoleteJunctions) = obsoleteJunctionPoints.partition(
      jp => obsoleteJunctions.filter(j => j.id == jp.junctionId).isEmpty)

    // Create junction rows with end date and junction point rows with end date and new junction id
    obsoleteJunctions.foreach(j => {
      val newJunctionId = junctionDAO.create(Seq(j.copy(id = NewIdValue, endDate = endDate, createdBy = Some(username)))).head
      junctionPointDAO.create(obsoleteJunctionPointsOfObsoleteJunctions.map(_.copy(id = NewIdValue, endDate = endDate,
        junctionId = newJunctionId, createdBy = Some(username))))
    })

    // Create junction point rows of the valid junctions with end date
    junctionPointDAO.create(obsoleteJunctionPointsOfValidJunctions.map(_.copy(id = NewIdValue, endDate = endDate, createdBy = Some(username))))

  }

}
