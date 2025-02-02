package fi.liikennevirasto.viite.dao

import fi.liikennevirasto.digiroad2.Point
import fi.liikennevirasto.digiroad2.asset.BoundingRectangle
import fi.liikennevirasto.digiroad2.dao.Sequences
import fi.liikennevirasto.digiroad2.postgis.PostGISDatabase
import fi.liikennevirasto.digiroad2.util.LogUtils.time
import fi.liikennevirasto.digiroad2.util.Track
import fi.liikennevirasto.viite.NewIdValue
import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormatter, ISODateTimeFormat}
import slick.driver.JdbcDriver.backend.Database.dynamicSession
import slick.jdbc.{GetResult, PositionedResult, StaticQuery => Q}

case class Junction(id: Long, junctionNumber: Option[Long], nodeNumber: Option[Long], startDate: DateTime, endDate: Option[DateTime],
                    validFrom: DateTime, validTo: Option[DateTime], createdBy: String, createdTime: Option[DateTime],
                    junctionPoints: Option[List[JunctionPoint]] = None)

case class JunctionInfo(id: Long, junctionNumber: Option[Long], startDate: DateTime, nodeNumber: Long, nodeName: String)

case class JunctionTemplate(id: Long, startDate: DateTime, roadNumber: Long, roadPartNumber: Long, track: Track, addrM: Long, elyCode: Long, coords: Point = Point(0.0, 0.0))

class JunctionDAO extends BaseDAO {

  val dateFormatter: DateTimeFormatter = ISODateTimeFormat.basicDate()

  implicit val getJunction: GetResult[Junction] = new GetResult[Junction] {
    def apply(r: PositionedResult): Junction = {
      val id = r.nextLong()
      val junctionNumber = r.nextLongOption()
      val nodeNumber = r.nextLongOption()
      val startDate = formatter.parseDateTime(r.nextDate.toString)
      val endDate = r.nextDateOption.map(d => formatter.parseDateTime(d.toString))
      val validFrom = formatter.parseDateTime(r.nextDate.toString)
      val validTo = r.nextDateOption.map(d => formatter.parseDateTime(d.toString))
      val createdBy = r.nextString()
      val createdTime = r.nextDateOption.map(d => formatter.parseDateTime(d.toString))
      Junction(id, junctionNumber, nodeNumber, startDate, endDate, validFrom, validTo, createdBy, createdTime)
    }
  }

  implicit val getJunctionTemplate: GetResult[JunctionTemplate] = new GetResult[JunctionTemplate] {
    def apply(r: PositionedResult): JunctionTemplate = {
      val junctionId = r.nextLong()
      val startDate = formatter.parseDateTime(r.nextDate.toString)
      val roadNumber = r.nextLong()
      val roadPartNumber = r.nextLong()
      val trackCode = r.nextInt()
      val addrM = r.nextLong()
      val ely = r.nextLong()

      JunctionTemplate(junctionId, startDate, roadNumber, roadPartNumber, Track.apply(trackCode), addrM, ely)
    }
  }

  implicit val getJunctionInfo: GetResult[JunctionInfo] = new GetResult[JunctionInfo] {
    def apply(r: PositionedResult): JunctionInfo = {
      val id = r.nextLong()
      val junctionNumber = r.nextLongOption()
      val nodeNumber = r.nextLong()
      val startDate = formatter.parseDateTime(r.nextDate.toString)
      val nodeName = r.nextString()

      JunctionInfo(id, junctionNumber, startDate, nodeNumber, nodeName)
    }
  }

  private def queryList(query: String): List[Junction] = {
    Q.queryNA[Junction](query).list.groupBy(_.id).map {
      case (_, list) =>
        list.head
    }.toList
  }

  private def queryListTemplate(query: String): List[JunctionTemplate] = {
    Q.queryNA[JunctionTemplate](query).list.groupBy(_.id).map {
      case (_, list) =>
        list.minBy(jt => (jt.roadNumber, jt.roadPartNumber, jt.addrM))
    }.toList
  }

  def fetchJunctionByNodeNumber(nodeNumber: Long): Seq[Junction] = {
    fetchJunctionsByNodeNumbers(Seq(nodeNumber))
  }

  def fetchJunctionsByNodeNumbers(nodeNumbers: Seq[Long]): Seq[Junction] = {
    if (nodeNumbers.isEmpty) {
      Seq()
    } else {
      val query = s"""
        SELECT ID, JUNCTION_NUMBER, NODE_NUMBER, START_DATE, END_DATE, VALID_FROM, VALID_TO, CREATED_BY, CREATED_TIME
          FROM JUNCTION
          where NODE_NUMBER in (${nodeNumbers.mkString(", ")}) AND VALID_TO IS NULL AND END_DATE IS NULL
        """
      queryList(query)
    }
  }

  def fetchByIds(ids: Seq[Long]): Seq[Junction] = {
    if (ids.isEmpty)
      List()
    else {
      val query =
        s"""
      SELECT ID, JUNCTION_NUMBER, NODE_NUMBER, START_DATE, END_DATE, VALID_FROM, VALID_TO, CREATED_BY, CREATED_TIME
      FROM JUNCTION
      WHERE ID IN (${ids.mkString(", ")}) AND VALID_TO IS NULL AND END_DATE IS NULL
      """
      queryList(query)
    }
  }

  def fetchAllByIds(ids: Seq[Long]): Seq[Junction] = {
    if (ids.isEmpty)
      List()
    else {
      val query =
        s"""
      SELECT ID, JUNCTION_NUMBER, NODE_NUMBER, START_DATE, END_DATE, VALID_FROM, VALID_TO, CREATED_BY, CREATED_TIME
      FROM JUNCTION
      WHERE ID IN (${ids.mkString(", ")})
      """
      queryList(query)
    }
  }

  def fetchJunctionByIdWithValidPoints(id: Long): Seq[Junction] = {
      val query =
        s"""
      SELECT j.ID, j.JUNCTION_NUMBER, j.NODE_NUMBER, j.START_DATE, j.END_DATE, j.VALID_FROM, j.VALID_TO, j.CREATED_BY, j.CREATED_TIME
      FROM JUNCTION j
      INNER JOIN JUNCTION_POINT jp ON j.ID = jp.JUNCTION_ID AND jp.VALID_TO IS NULL
      WHERE j.ID = $id AND j.VALID_TO IS NULL AND j.END_DATE IS NULL
      """
      queryList(query)
  }

  def fetchTemplates() : Seq[JunctionTemplate] = {
    val query =
      s"""
         SELECT DISTINCT j.ID, j.START_DATE, rw.ROAD_NUMBER, rw.ROAD_PART_NUMBER, rw.TRACK, rp.ADDR_M, rw.ELY
         FROM JUNCTION j
         JOIN JUNCTION_POINT jp ON j.ID = jp.JUNCTION_ID AND jp.VALID_TO IS NULL
         JOIN ROADWAY_POINT rp ON jp.ROADWAY_POINT_ID = rp.ID
         JOIN ROADWAY rw ON rp.ROADWAY_NUMBER = rw.ROADWAY_NUMBER AND rw.VALID_TO IS NULL AND rw.END_DATE IS NULL
            WHERE j.VALID_TO IS NULL AND j.END_DATE IS NULL AND j.NODE_NUMBER IS NULL
       """
    queryListTemplate(query)
  }

  def fetchJunctionTemplateById(id: Long): Option[JunctionTemplate] = {
    val query =
      s"""
         SELECT DISTINCT j.ID, j.START_DATE, rw.ROAD_NUMBER, rw.ROAD_PART_NUMBER, rw.TRACK, rp.ADDR_M, rw.ELY
         FROM JUNCTION j
         JOIN JUNCTION_POINT jp ON j.ID = jp.JUNCTION_ID AND jp.VALID_TO IS NULL
         JOIN ROADWAY_POINT rp ON jp.ROADWAY_POINT_ID = rp.ID
         JOIN ROADWAY rw ON rp.ROADWAY_NUMBER = rw.ROADWAY_NUMBER AND rw.VALID_TO IS NULL AND rw.END_DATE IS NULL
            WHERE j.VALID_TO IS NULL AND j.END_DATE IS NULL AND j.NODE_NUMBER IS NULL
            AND j.id = $id
       """
    queryListTemplate(query).headOption
  }

  def fetchTemplatesByRoadwayNumbers(roadwayNumbers: Iterable[Long]) : Seq[JunctionTemplate] = {
    if (roadwayNumbers.nonEmpty) {
      val query =
        s"""
         SELECT DISTINCT j.ID, j.START_DATE, rw.ROAD_NUMBER, rw.ROAD_PART_NUMBER, rw.TRACK, rp.ADDR_M, rw.ELY
         FROM JUNCTION j
         JOIN JUNCTION_POINT jp ON j.ID = jp.JUNCTION_ID AND jp.VALID_TO IS NULL
         JOIN ROADWAY_POINT rp ON jp.ROADWAY_POINT_ID = rp.ID
         JOIN ROADWAY rw ON rp.ROADWAY_NUMBER = rw.ROADWAY_NUMBER AND rw.VALID_TO IS NULL AND rw.END_DATE IS NULL
         WHERE j.VALID_TO IS NULL AND j.END_DATE IS NULL AND j.NODE_NUMBER IS NULL
           AND rw.ROADWAY_NUMBER IN (${roadwayNumbers.mkString(", ")})
       """
      queryListTemplate(query)
    } else {
      Seq.empty[JunctionTemplate]
    }
  }

  def fetchExpiredByRoadwayNumbers(roadwayNumbers: Iterable[Long]) : Seq[Junction] = {
    if (roadwayNumbers.nonEmpty) {
      val query =
        s"""
         SELECT j.ID, j.JUNCTION_NUMBER, j.NODE_NUMBER, j.START_DATE, j.END_DATE, j.VALID_FROM, j.VALID_TO, j.CREATED_BY, j.CREATED_TIME
         FROM JUNCTION j
         JOIN JUNCTION_POINT jp ON j.ID = jp.JUNCTION_ID
         JOIN ROADWAY_POINT rp ON jp.ROADWAY_POINT_ID = rp.ID
         JOIN ROADWAY rw ON rp.ROADWAY_NUMBER = rw.ROADWAY_NUMBER
         WHERE rw.ROADWAY_NUMBER IN (${roadwayNumbers.mkString(", ")})
        """
      queryList(query)
    } else {
      Seq.empty[Junction]
    }
  }

  def fetchByBoundingBox(boundingRectangle: BoundingRectangle): Seq[Junction] = {
    time(logger, "Fetch Junction templates by bounding box") {
      val extendedBoundingRectangle = BoundingRectangle(boundingRectangle.leftBottom + boundingRectangle.diagonal.scale(.15),
        boundingRectangle.rightTop - boundingRectangle.diagonal.scale(.15))

      val boundingBoxFilter = PostGISDatabase.boundingBoxFilter(extendedBoundingRectangle, "LL.geometry")

      val query =
        s"""
         SELECT j.ID, j.JUNCTION_NUMBER, j.NODE_NUMBER, j.START_DATE, j.END_DATE, j.VALID_FROM, j.VALID_TO, j.CREATED_BY, j.CREATED_TIME
         FROM JUNCTION j
         JOIN JUNCTION_POINT jp ON j.ID = jp.JUNCTION_ID AND jp.VALID_TO IS NULL
         JOIN ROADWAY_POINT rp ON jp.ROADWAY_POINT_ID = rp.ID
         JOIN LINEAR_LOCATION LL ON (LL.ROADWAY_NUMBER = RP.ROADWAY_NUMBER AND LL.VALID_TO IS NULL)
         JOIN ROADWAY rw ON rp.ROADWAY_NUMBER = rw.ROADWAY_NUMBER AND rw.VALID_TO IS NULL AND rw.END_DATE IS NULL
            WHERE j.VALID_TO IS NULL AND j.END_DATE IS NULL
            AND $boundingBoxFilter and JP.valid_to is null
            AND j.NODE_NUMBER IS NOT NULL
        """
      queryList(query)
    }
  }

  def fetchTemplatesByBoundingBox(boundingRectangle: BoundingRectangle): Seq[JunctionTemplate] = {
    time(logger, "Fetch Junction templates by bounding box") {
      val extendedBoundingRectangle = BoundingRectangle(boundingRectangle.leftBottom + boundingRectangle.diagonal.scale(.15),
        boundingRectangle.rightTop - boundingRectangle.diagonal.scale(.15))

      val boundingBoxFilter = PostGISDatabase.boundingBoxFilter(extendedBoundingRectangle, "LL.geometry")

      val query =
        s"""
         SELECT DISTINCT j.ID, j.START_DATE, rw.ROAD_NUMBER, rw.ROAD_PART_NUMBER, rw.TRACK, rp.ADDR_M, rw.ELY
         FROM JUNCTION j
         JOIN JUNCTION_POINT jp ON j.ID = jp.JUNCTION_ID AND jp.VALID_TO IS NULL
         JOIN ROADWAY_POINT rp ON jp.ROADWAY_POINT_ID = rp.ID
         JOIN LINEAR_LOCATION LL ON (LL.ROADWAY_NUMBER = RP.ROADWAY_NUMBER AND LL.VALID_TO IS NULL)
         JOIN ROADWAY rw ON rp.ROADWAY_NUMBER = rw.ROADWAY_NUMBER AND rw.VALID_TO IS NULL AND rw.END_DATE IS NULL
            WHERE j.VALID_TO IS NULL AND j.END_DATE IS NULL AND j.NODE_NUMBER IS NULL
            AND $boundingBoxFilter and JP.valid_to is null
        """
      queryListTemplate(query)
    }
  }

  def create(junctions: Iterable[Junction]): Seq[Long] = {

    val ps = dynamicSession.prepareStatement(
      """insert into JUNCTION (ID, JUNCTION_NUMBER, NODE_NUMBER, START_DATE, END_DATE, CREATED_BY)
      values (?, ?, ?, ?, ?, ?)""".stripMargin)

    // Set ids for the junctions without one
    val (ready, idLess) = junctions.partition(_.id != NewIdValue)
    val newIds = Sequences.fetchJunctionIds(idLess.size)
    val createJunctions = ready ++ idLess.zip(newIds).map(x =>
      x._1.copy(id = x._2)
    )

    createJunctions.foreach {
      junction =>
        ps.setLong(1, junction.id)
        if (junction.junctionNumber.isDefined) {
          ps.setLong(2, junction.junctionNumber.get)
        } else {
          ps.setNull(2, java.sql.Types.INTEGER)
        }
        if (junction.nodeNumber.isDefined) {
          ps.setLong(3, junction.nodeNumber.get)
        } else {
          ps.setNull(3, java.sql.Types.INTEGER)
        }
        ps.setDate(4, new java.sql.Date(junction.startDate.getMillis))
        if (junction.endDate.isDefined) {
           ps.setDate(5, new java.sql.Date(junction.endDate.get.getMillis))
        } else {
          ps.setNull(5, java.sql.Types.DATE)
        }
        ps.setString(6, junction.createdBy)
        ps.addBatch()
    }
    ps.executeBatch()
    ps.close()
    createJunctions.map(_.id).toSeq
  }

  /**
    * Expires junctions (set their valid_to to the current system date).
    *
    * @param ids : Iterable[Long] - The ids of the junctions to expire.
    * @return
    */
  def expireById(ids: Iterable[Long]): Int = {
    if (ids.isEmpty) 0
    else {
      val query = s"""UPDATE JUNCTION SET VALID_TO = CURRENT_TIMESTAMP WHERE VALID_TO IS NULL AND ID IN (${ids.mkString(", ")})"""
      Q.updateNA(query).first
    }
  }

}
