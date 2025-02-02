package fi.liikennevirasto.viite.process

import fi.liikennevirasto.digiroad2.GeometryUtils
import fi.liikennevirasto.digiroad2.linearasset.RoadLinkLike
import fi.liikennevirasto.digiroad2.dao.Sequences
import fi.liikennevirasto.digiroad2.service.RoadLinkService
import fi.liikennevirasto.digiroad2.util.LogUtils.time
import fi.liikennevirasto.viite.dao.{RoadAddress, RoadNetworkDAO, RoadwayDAO}
import fi.liikennevirasto.viite.{MaxMoveDistanceBeforeFloating, RoadCheckOptions, RoadNetworkService}
import org.slf4j.LoggerFactory

class RoadNetworkChecker(roadLinkService: RoadLinkService) {
  val logger = LoggerFactory.getLogger(getClass)

  def checkRoadNetwork(username: String = ""): Unit = {
    time(logger, "Validation of road network") {
      val roadNetworkService = new RoadNetworkService {
        override def withDynTransaction[T](f: => T): T = f
      }
      val roadNetworkDAO = new RoadNetworkDAO
      val roadwayDAO = new RoadwayDAO

      if (roadNetworkDAO.hasCurrentNetworkErrors) {
        logger.error(s"current network has errors")
      } else {
        val roadNumbers = roadwayDAO.getValidRoadNumbers
        val chunks = generateChunks(roadNumbers, 500)
        roadNetworkDAO.getLatestRoadNetworkVersionId.getOrElse(
          roadNetworkDAO.createPublishedRoadNetwork()
        )
        val currNetworkVersion = roadNetworkDAO.getLatestRoadNetworkVersionId
        val nextNetworkVersion = Sequences.nextPublishedRoadNetworkId
        chunks.foreach {
          case (min, max) =>
            val roads = roadwayDAO.getValidBetweenRoadNumbers((min.toLong, max.toLong))
            roadNetworkService.checkRoadAddressNetwork(RoadCheckOptions(Seq(), roads.toSet, currNetworkVersion, nextNetworkVersion, throughActor = false))
        }
        if (!roadNetworkDAO.hasCurrentNetworkErrors) {
          logger.info(s"No errors found. Creating new publishable version for the road network")
          roadNetworkDAO.expireRoadNetwork
          roadNetworkDAO.createPublishedRoadNetwork(nextNetworkVersion)
          val newId = roadNetworkDAO.getLatestRoadNetworkVersionId
          roadwayDAO.fetchAllCurrentAndValidRoadwayIds.foreach(id => roadNetworkDAO.createPublishedRoadway(newId.get, id))
        } else {
          logger.info(s"Network errors found. Check road_network_error table")
        }
      }
    }
  }

  /**
    * Check if road address geometry is moved by road link geometry change at least MaxMoveDistanceBeforeFloating
    * meters. Because road address geometry isn't directed check for fit either way. Public for testing.
    *
    * @param roadLink      Road link for road address list
    * @param roadAddresses Sequence of road addresses to check
    * @return true, if geometry has changed for any of the addresses beyond tolerance
    */
  def isGeometryChange(roadLink: RoadLinkLike, roadAddresses: Seq[RoadAddress]): Boolean = {
    val movedAddresses = roadAddresses.filter(ra => {
      GeometryUtils.geometryMoved(MaxMoveDistanceBeforeFloating)(
        GeometryUtils.truncateGeometry2D(roadLink.geometry, ra.startMValue, ra.endMValue),
        ra.geometry) &&
        GeometryUtils.geometryMoved(MaxMoveDistanceBeforeFloating)(
          GeometryUtils.truncateGeometry2D(roadLink.geometry, ra.startMValue, ra.endMValue),
          ra.geometry.reverse) // Road Address geometry isn't necessarily directed: start and end may not be aligned by side code
    }
    )
    val checkMaxMovedDistance = Math.abs(roadAddresses.maxBy(_.endMValue).endMValue - GeometryUtils.geometryLength(roadLink.geometry)) > MaxMoveDistanceBeforeFloating
    if (movedAddresses.nonEmpty) {
      println(s"The following road addresses (${movedAddresses.map(_.id).mkString(", ")}) deviate by a factor of $MaxMoveDistanceBeforeFloating of the RoadLink: ${roadLink.linkId}")
      println(s"Proceeding to check if the addresses are a result of automatic merging and if they overlap.")

      // If we get road addresses that were merged we check if they current road link is not overlapping, if it not, then there is a floating problem
      val filteredNonOverlapping = movedAddresses.filterNot(ma => {
        val filterResult = ma.createdBy.getOrElse("") == "Automatic_merged" && GeometryUtils.overlaps((ma.startMValue, ma.endMValue), (0.0, roadLink.length))
        if (filterResult) {
          println(s"Road address ${ma.id} is a result of automatic merging and it overlaps, discarding.")
        }
        filterResult
      })
      filteredNonOverlapping.nonEmpty || checkMaxMovedDistance

    } else {
      movedAddresses.nonEmpty || checkMaxMovedDistance
    }

  }

  private def generateChunks(roadNumbers: List[Long], chunkNumber: Long): Seq[(Long, Long)] = {
    val (chunks, _) = roadNumbers.foldLeft((Seq[Long](0), 0)) {
      case ((fchunks, index), roadNumber) =>
        if (index > 0 && index % chunkNumber == 0) {
          (fchunks ++ Seq(roadNumber), index + 1)
        } else {
          (fchunks, index + 1)
        }
    }
    val result = if (chunks.last == roadNumbers.last) {
      chunks
    } else {
      chunks ++ Seq(roadNumbers.last)
    }

    result.zip(result.tail)
  }

}
