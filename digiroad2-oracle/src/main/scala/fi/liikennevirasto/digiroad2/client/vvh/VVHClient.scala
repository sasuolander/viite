package fi.liikennevirasto.digiroad2.client.vvh

import java.io.IOException
import java.net.URLEncoder
import java.util
import fi.liikennevirasto.digiroad2.Point
import fi.liikennevirasto.digiroad2.asset._
import fi.liikennevirasto.digiroad2.client.vvh.ChangeType.{Unknown => _, _}
import fi.liikennevirasto.digiroad2.linearasset.RoadLinkLike
import fi.liikennevirasto.digiroad2.util.LogUtils.time
import fi.liikennevirasto.digiroad2.util.ViiteProperties
import org.apache.commons.codec.binary.Base64
import org.apache.http.{HttpHeaders, NameValuePair}
import org.apache.http.client.entity.UrlEncodedFormEntity
import org.apache.http.client.methods.{CloseableHttpResponse, HttpGet, HttpPost}
import org.apache.http.impl.client.HttpClientBuilder
import org.apache.http.message.BasicNameValuePair
import org.joda.time.format.DateTimeFormat
import org.joda.time.{DateTime, DateTimeZone}
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

sealed trait FeatureClass
object FeatureClass {
  case object TractorRoad extends FeatureClass
  case object DrivePath extends FeatureClass
  case object CycleOrPedestrianPath extends FeatureClass
  case object WinterRoads extends FeatureClass
  case object AllOthers extends FeatureClass
}

case class VVHRoadlink(linkId: Long, municipalityCode: Int, geometry: Seq[Point],
                       administrativeClass: AdministrativeClass, trafficDirection: TrafficDirection,
                       featureClass: FeatureClass, modifiedAt: Option[DateTime] = None, attributes: Map[String, Any] = Map(),
                       constructionType: ConstructionType = ConstructionType.InUse, linkSource: LinkGeomSource = LinkGeomSource.NormalLinkInterface, length: Double = 0.0) extends RoadLinkLike {
  def roadNumber: Option[String] = attributes.get("ROADNUMBER").map(_.toString)
  val vvhTimeStamp: Long = attributes.getOrElse("LAST_EDITED_DATE", attributes.getOrElse("CREATED_DATE", BigInt(0))).asInstanceOf[BigInt].longValue()
}

case class ChangeInfo(oldId: Option[Long], newId: Option[Long], mmlId: Long, changeType: ChangeType,
                      oldStartMeasure: Option[Double], oldEndMeasure: Option[Double], newStartMeasure: Option[Double],
                      newEndMeasure: Option[Double], vvhTimeStamp: Long = 0L) {
  def isOldId(id: Long): Boolean = {
    oldId.nonEmpty && oldId.get == id
  }

  def affects(id: Long, assetVvhTimeStamp: Long): Boolean = {
    isOldId(id) && assetVvhTimeStamp < vvhTimeStamp
  }
}

case class VVHHistoryRoadLink(linkId: Long, municipalityCode: Int, geometry: Seq[Point], administrativeClass: AdministrativeClass,
                              trafficDirection: TrafficDirection, featureClass: FeatureClass, createdDate: BigInt, endDate: BigInt, attributes: Map[String, Any] = Map(),
                              constructionType: ConstructionType = ConstructionType.InUse, linkSource: LinkGeomSource = LinkGeomSource.NormalLinkInterface, length: Double = 0.0) extends RoadLinkLike {
  def roadNumber: Option[String] = attributes.get("ROADNUMBER").map(_.toString)

  val vvhTimeStamp: Long = attributes.getOrElse("LAST_EDITED_DATE", createdDate).asInstanceOf[BigInt].longValue()
}

/**
  * Numerical values for change types from VVH ChangeInfo Api
  */
sealed trait ChangeType {
  def value: Int

  def isShortenedChangeType: Boolean = {
    ChangeType.apply(value) match {
      case ShortenedCommonPart => true
      case ShortenedRemovedPart => true
      case _ => false
    }
  }

  def isLengthenedChangeType: Boolean = {
    ChangeType.apply(value) match {
      case LengthenedCommonPart => true
      case LengthenedNewPart => true
      case _ => false
    }
  }

  def isDividedChangeType: Boolean = {
    ChangeType.apply(value) match {
      case DividedNewPart => true
      case DividedModifiedPart => true
      case _ => false
    }
  }

  def isCombinedChangeType: Boolean = {
    ChangeType.apply(value) match {
      case CombinedModifiedPart => true
      case CombinedRemovedPart => true
      case _ => false
    }
  }
}

object ChangeType {
  val values = Set(Unknown, CombinedModifiedPart, CombinedRemovedPart, LengthenedCommonPart, LengthenedNewPart, DividedModifiedPart, DividedNewPart, ShortenedCommonPart, ShortenedRemovedPart, Removed, New, ReplacedCommonPart, ReplacedNewPart, ReplacedRemovedPart)

  def apply(intValue: Int): ChangeType = {
    values.find(_.value == intValue).getOrElse(Unknown)
  }

  case object Unknown extends ChangeType { def value = 0 }

  case object CombinedModifiedPart extends ChangeType { def value = 1 }
  case object CombinedRemovedPart extends ChangeType { def value = 2 }

  case object LengthenedCommonPart extends ChangeType { def value = 3 }
  case object LengthenedNewPart extends ChangeType { def value = 4 }

  case object DividedModifiedPart extends ChangeType { def value = 5 }
  case object DividedNewPart extends ChangeType { def value = 6 }

  case object ShortenedCommonPart extends ChangeType { def value = 7 }
  case object ShortenedRemovedPart extends ChangeType { def value = 8 }

  case object Removed extends ChangeType { def value = 11 }
  case object New extends ChangeType { def value = 12 }
  case object ReplacedCommonPart extends ChangeType { def value = 13 }
  case object ReplacedNewPart extends ChangeType { def value = 14 }
  case object ReplacedRemovedPart extends ChangeType { def value = 16 }

  /**
    * Return true if this is a replacement where segment or part of it replaces another, older one
    * All changes should be of form (old_id, new_id, old_start, old_end, new_start, new_end) with non-null values
    *
    * @param changeInfo changeInfo object to check
    * @return true, if this is a replacement
    */
  def isReplacementChange(changeInfo: ChangeInfo): Boolean = { // Where asset geo location should be replaced with another
    changeInfo.changeType match {
      case CombinedModifiedPart => true
      case CombinedRemovedPart => true
      case LengthenedCommonPart => true
      case DividedModifiedPart => true
      case DividedNewPart => true
      case ShortenedCommonPart => true
      case ReplacedCommonPart => true
      case Unknown => false
      case LengthenedNewPart => false
      case ShortenedRemovedPart => false
      case Removed => false
      case New => false
      case ReplacedNewPart => false
      case ReplacedRemovedPart => false
    }
  }

  /**
    * Return true if this is an extension where segment or part of it has no previous entry
    * All changes should be of form (new_id, new_start, new_end) with non-null values and old_* fields must be null
    *
    * @param changeInfo changeInfo object to check
    * @return true, if this is an extension
    */
  def isExtensionChange(changeInfo: ChangeInfo): Boolean = { // Where asset geo location is a new extension (non-existing)
    changeInfo.changeType match {
      case LengthenedNewPart => true
      case ReplacedNewPart => true
      case _ => false
    }
  }

  /**
    * Return true if this is a removed segment or a piece of it. Only old id and m-values should be populated.
    *
    * @param changeInfo changeInfo object to check
    * @return true, if this is a removed segment
    */
  def isRemovalChange(changeInfo: ChangeInfo): Boolean = { // Where asset should be removed completely or partially
    changeInfo.changeType match {
      case Removed => true
      case ReplacedRemovedPart => true
      case ShortenedRemovedPart => true
      case _ => false
    }
  }

  /**
    * Return true if this is a new segment. Only new id and m-values should be populated.
    *
    * @param changeInfo changeInfo object to check
    * @return true, if this is a new segment
    */
  def isCreationChange(changeInfo: ChangeInfo): Boolean = { // Where asset geo location should be replaced with another
    changeInfo.changeType match {
      case New => true
      case _ => false
    }
  }

  def isUnknownChange(changeInfo: ChangeInfo): Boolean = {
    ChangeType.Unknown == changeInfo.changeType
  }
}

object VVHClient {
  /**
    * Create a pseudo VVH time stamp when an asset is created or updated and is on the current road geometry.
    * This prevents change info from being applied to the recently created asset. Resolution is one day.
    *
    * @param offsetHours Offset to the timestamp. Defaults to 5 which reflects to VVH offset for batch runs.
    * @return VVH timestamp for current date
    */
  def createVVHTimeStamp(offsetHours: Int = 5): Long = {
    val oneHourInMs = 60 * 60 * 1000L
    val utcTime = DateTime.now().minusHours(offsetHours).getMillis
    val curr = utcTime + DateTimeZone.getDefault.getOffset(utcTime)
    curr - (curr % (24L * oneHourInMs))
  }

  val featureClassCodeToFeatureClass: Map[Int, FeatureClass] = Map(
    12316 -> FeatureClass.TractorRoad,
    12141 -> FeatureClass.DrivePath,
    12314 -> FeatureClass.CycleOrPedestrianPath,
    12312 -> FeatureClass.WinterRoads
  )
}

class VVHClient(vvhRestApiEndPoint: String) {
  lazy val roadLinkData: VVHRoadLinkClient = new VVHRoadLinkClient(vvhRestApiEndPoint)
  lazy val frozenTimeRoadLinkData: VVHRoadLinkClient = new VVHFrozenTimeRoadLinkClientServicePoint(vvhRestApiEndPoint)
  lazy val roadLinkChangeInfo: VVHChangeInfoClient = new VVHChangeInfoClient(vvhRestApiEndPoint)
  lazy val complementaryData: VVHComplementaryClient = new VVHComplementaryClient(vvhRestApiEndPoint)
  lazy val historyData: VVHHistoryClient = new VVHHistoryClient(vvhRestApiEndPoint)

  def fetchRoadLinkByLinkId(linkId: Long): Option[VVHRoadlink] = {
    roadLinkData.fetchByLinkId(linkId) match {
      case Some(vvhRoadLink) => Some(vvhRoadLink)
      case None => complementaryData.fetchByLinkId(linkId)
    }
  }

  def createVVHTimeStamp(offsetHours: Int = 5): Long = {
    VVHClient.createVVHTimeStamp(offsetHours)
  }
}

trait VVHClientOperations {

  type VVHType

  protected val linkGeomSource: LinkGeomSource

  protected def restApiEndPoint: String

  protected def serviceName: String

  protected val disableGeometry: Boolean

  case class VVHError(content: Map[String, Any], url: String)

  class VVHClientException(response: String) extends RuntimeException(response)

  protected implicit val jsonFormats: Formats = DefaultFormats

  protected def mapFields(content: Map[String, Any], url: String): Either[VVHError, List[Map[String, Any]]]

  protected def defaultOutFields(): String

  protected def extractVVHFeature(feature: Map[String, Any]): VVHType

  lazy val logger: Logger = LoggerFactory.getLogger(getClass)

  protected def anyToDouble(number: Any): Option[Double] = number match {
    case bi: BigInt => Some(bi.toDouble)
    case i: Int => Some(i.toDouble)
    case l: Long => Some(l.toDouble)
    case d: Double => Some(d)
    case _ => None
  }

  protected def withFilter[T](attributeName: String, ids: Set[T]): String = {
    val filter =
      if (ids.isEmpty) {
        ""
      } else {
        val query = ids.mkString(",")
        s""""where":"$attributeName IN ($query)","""
      }
    filter
  }

  protected def withLimitFilter(attributeName: String, low: Int, high: Int, includeAllPublicRoads: Boolean = false): String = {
    val filter =
      if (low < 0 || high < 0 || low > high) {
        ""
      } else {
        if (includeAllPublicRoads) {
          //TODO check if we can remove the adminclass in the future
          s""""where":"( ADMINCLASS = 1 OR $attributeName >= $low and $attributeName <= $high )","""
        } else {
          s""""where":"( $attributeName >= $low and $attributeName <= $high )","""
        }
      }
    filter
  }

  protected def withMunicipalityFilter(municipalities: Set[Int]): String = {
    withFilter("MUNICIPALITYCODE", municipalities)
  }

  protected def combineFiltersWithAnd(filter1: String, filter2: String): String = {

    (filter1.isEmpty, filter2.isEmpty) match {
      case (true, true) => ""
      case (true, false) => filter2
      case (false, true) => filter1
      case (false, false) => "%s AND %s".format(filter1.dropRight(2), filter2.replace("\"where\":\"", ""))
    }
  }

  protected def combineFiltersWithAnd(filter1: String, filter2: Option[String]): String = {
    combineFiltersWithAnd(filter2.getOrElse(""), filter1)
  }

  protected def queryParameters(fetchGeometry: Boolean = true): String = {
    if (fetchGeometry && !disableGeometry) "returnGeometry=true&returnZ=true&returnM=true&geometryPrecision=3&f=json"
    else "returnGeometry=false&f=json"
  }

  protected def serviceUrl: String = restApiEndPoint + serviceName + "/FeatureServer/query"

  protected def serviceUrl(bounds: BoundingRectangle, definition: String, parameters: String): String = {
    serviceUrl +
      s"?layerDefs=$definition&geometry=" + bounds.leftBottom.x + "," + bounds.leftBottom.y + "," + bounds.rightTop.x + "," + bounds.rightTop.y +
      s"&geometryType=esriGeometryEnvelope&spatialRel=esriSpatialRelIntersects&$parameters"

  }

  protected def serviceUrl(definition: String, parameters: String): String = {
    serviceUrl +
      s"?layerDefs=$definition&" + parameters
  }

  protected def layerDefinitionWithoutEncoding(filter: String, customFieldSelection: Option[String] = None): String = {
    val definitionStart = "[{"
    val layerSelection = """"layerId":0,"""
    val fieldSelection = customFieldSelection match {
      case Some(fs) => s""""outFields":"""" + fs + """,CONSTRUCTIONTYPE""""
      case _ => s""""outFields":"""" + defaultOutFields + """""""
    }
    val definitionEnd = "}]"
    definitionStart + layerSelection + filter + fieldSelection + definitionEnd
  }

  protected def layerDefinition(filter: String, customFieldSelection: Option[String] = None): String = {
    URLEncoder.encode(layerDefinitionWithoutEncoding(filter, customFieldSelection), "UTF-8")
  }


  /** Tries up to ten times to fetch the given <i>url</i> from VVH.
    * @return Right(mapFields---) in success, or an Left(VVHError) at the end of the tenth failed try.
    */
  protected def fetchVVHFeatures(url: String): Either[VVHError, List[Map[String, Any]]] = {
    val MaxTries = 10
    var trycounter = 0 /// For do-while check. Up to MaxTries.
    var success = true /// For do-while check. Set to false when exception.
    var result : Either[VVHError, List[Map[String, Any]]] = Left(VVHError(Map(("Dummy start value", "nothing here")), url))

    val auth: String = ViiteProperties.vvhRestApiUsername + ":" + ViiteProperties.vvhRestApiPassword
    val encodedAuth: String = Base64.encodeBase64String(auth.getBytes)

    do {
      trycounter += 1
      success = true
      result = time(logger, s"Fetch VVH features, (try $trycounter)") {
        val request = new HttpGet(url)
        request.addHeader(HttpHeaders.AUTHORIZATION, "Basic " + encodedAuth)

        val client = HttpClientBuilder.create().build()
        var response: CloseableHttpResponse = null

        try {
          response = client.execute(request)
          mapFields(parse(StreamInput(response.getEntity.getContent)).values.asInstanceOf[Map[String, Any]], url)
        } catch {
          case e: IOException => {
            success = false
            // Before the last possible try, log the failure. If the last try, just return the error.
            if (trycounter < MaxTries) {
              logger.warn(s"fetching $url failed, try $trycounter. IO Exception during VVH fetch. Exception: $e")
              Left(VVHError(Map((
                s"VVH FETCH failure, try $trycounter.",
                "IO Exception during VVH fetch. Trying again.")), url))
            }
            else // basically, if(trycounter == MaxTries)
              Left(VVHError(Map((
                "VVH FETCH failure, tried ten (10) times, giving up.",
                "IO Exception during VVH fetch. Check connection to VVH")), url))
          }
        } finally {
          if (response != null) {
            response.close()
          }
        }
      }
      result match {
        case Left(a) => println(s"entering round ${trycounter +1}")
        case Right(b) => Unit
      }

    } while (trycounter < MaxTries && !success)
    result
  }

  protected def fetchFeaturesAndLog(url: String): Seq[VVHType] = {
    fetchVVHFeatures(url) match {
      case Right(features) => features.map(extractVVHFeature)
      case Left(error) =>
        logger.error("VVH error: " + error)
        throw new VVHClientException(error.toString)
    }
  }

  protected def fetchVVHFeatures(url: String, formparams: util.ArrayList[NameValuePair]): Either[VVHError, List[Map[String, Any]]] = {
    time(logger, s"Fetch VVH features with url '$url'") {
      val request = new HttpPost(url)
      request.setEntity(new UrlEncodedFormEntity(formparams, "utf-8"))
      val client = HttpClientBuilder.create().build()
      try {
        val response = client.execute(request)
        try {
          mapFields(parse(StreamInput(response.getEntity.getContent)).values.asInstanceOf[Map[String, Any]], url)
        } finally {
          response.close()
          if (response.getStatusLine.getStatusCode >= 300) {
          return  Left(VVHError(Map(("VVH FETCH failure", "VVH response code was <300 (unsuccessful)")), url))
          }
        }
      } catch {
        case _: IOException => Left(VVHError(Map(("VVH FETCH failure", "IO Exception during VVH fetch. Check connection to VVH")), url))
      }
    }
  }

  protected def extractFeatureAttributes(feature: Map[String, Any]): Map[String, Any] = {
    feature("attributes").asInstanceOf[Map[String, Any]]
  }

  protected def extractFeatureGeometry(feature: Map[String, Any]): List[List[Double]] = {
    if (feature.contains("geometry")) {
      val geometry = feature("geometry").asInstanceOf[Map[String, Any]]
      val paths = geometry("paths").asInstanceOf[List[List[List[Double]]]]
      paths.reduceLeft((geom, nextPart) => geom ++ nextPart.tail)
    }
    else List.empty
  }


  protected def extractModifiedAt(attributes: Map[String, Any]): Option[DateTime] = {
    def compareDateMillisOptions(a: Option[Long], b: Option[Long]): Option[Long] = {
      (a, b) match {
        case (Some(firstModifiedAt), Some(secondModifiedAt)) =>
          Some(Math.max(firstModifiedAt, secondModifiedAt))
        case (Some(firstModifiedAt), None) => Some(firstModifiedAt)
        case (None, Some(secondModifiedAt)) => Some(secondModifiedAt)
        case (None, None) => None
      }
    }

    val validFromDate = Option(attributes("VALIDFROM").asInstanceOf[BigInt]).map(_.toLong)
    var lastEditedDate: Option[Long] = Option(0)
    if (attributes.contains("LAST_EDITED_DATE")) {
      lastEditedDate = Option(attributes("LAST_EDITED_DATE").asInstanceOf[BigInt]).map(_.toLong)
    }
    var geometryEditedDate: Option[Long] = Option(0)
    if (attributes.contains("GEOMETRY_EDITED_DATE")) {
      geometryEditedDate = Option(attributes("GEOMETRY_EDITED_DATE").asInstanceOf[BigInt]).map(_.toLong)
    }

    val latestDate = compareDateMillisOptions(lastEditedDate, geometryEditedDate)
    latestDate.orElse(validFromDate).map(modifiedTime => new DateTime(modifiedTime))
  }

  /**
    * Extract double value from VVH data. Used for change info start and end measures.
    */
  protected def extractMeasure(value: Any): Option[Double] = {
    value match {
      case null => None
      case _ => Some(value.toString.toDouble)
    }
  }

  /**
    * Creates a pseudo VVH Timestamp for new assets and speed limits. Turns clock back to 0:00 on the same day
    * if less than offsetHours have passed since or 0:00 on previous day if not.
    *
    * @param offsetHours Number of hours since midnight to return current day as a VVH timestamp (UNIX time in ms)
    */
  def createVVHTimeStamp(offsetHours: Int = 5): Long = {
    VVHClient.createVVHTimeStamp(offsetHours)
  }

  /**
    * Returns VVH road links by municipality.
    */
  protected def queryByMunicipality(municipality: Int, filter: Option[String] = None): Seq[VVHType] = {
    val definition = layerDefinition(combineFiltersWithAnd(withMunicipalityFilter(Set(municipality)), filter))
    val url = serviceUrl(definition, queryParameters())
    fetchFeaturesAndLog(url)
  }

  /**
    * Returns VVH road links in bounding box area. Municipalities are optional.
    */
  protected def queryByMunicipalitiesAndBounds(bounds: BoundingRectangle, municipalities: Set[Int], filter: Option[String]): Seq[VVHType] = {
    val definition = layerDefinition(combineFiltersWithAnd(withMunicipalityFilter(municipalities), filter))
    val url = serviceUrl(bounds, definition, queryParameters())
    fetchFeaturesAndLog(url)
  }

  protected def queryByMunicipalitiesAndBounds(bounds: BoundingRectangle, municipalities: Set[Int]): Seq[VVHType] = {
    queryByMunicipalitiesAndBounds(bounds, municipalities, None)
  }

}

class VVHFrozenTimeRoadLinkClientServicePoint(vvhRestApiEndPoint: String) extends VVHRoadLinkClient(vvhRestApiEndPoint) {
  protected override val serviceName = "Roadlink_temp"
  protected override val linkGeomSource: LinkGeomSource = LinkGeomSource.FrozenLinkInterface
  protected override val disableGeometry = false
}

class VVHRoadLinkClient(vvhRestApiEndPoint: String) extends VVHClientOperations {

  override type VVHType = VVHRoadlink

  protected override val restApiEndPoint: String = vvhRestApiEndPoint
  protected override val serviceName = "Roadlink_data"
  protected override val linkGeomSource: LinkGeomSource = LinkGeomSource.NormalLinkInterface
  protected override val disableGeometry = false

  protected override def defaultOutFields(): String = {
    "MTKID,LINKID,MUNICIPALITYCODE,MTKCLASS,ADMINCLASS,DIRECTIONTYPE,CONSTRUCTIONTYPE,ROADNAME_FI,ROADNAME_SM,ROADNAME_SE,LAST_EDITED_DATE,ROADNUMBER,ROADPARTNUMBER,VALIDFROM,GEOMETRY_EDITED_DATE,CREATED_DATE,GEOMETRYLENGTH"
  }

  protected override def mapFields(content: Map[String, Any], url: String): Either[VVHError, List[Map[String, Any]]] = {
    val optionalLayers = content.get("layers").map(_.asInstanceOf[List[Map[String, Any]]])
    val optionalFeatureLayer = optionalLayers.flatMap { layers => layers.find { layer => layer.contains("features") } }
    val optionalFeatures = optionalFeatureLayer.flatMap { featureLayer => featureLayer.get("features").map(_.asInstanceOf[List[Map[String, Any]]]) }
    optionalFeatures.map(_.filter(roadLinkStatusFilter)).map(Right(_)).getOrElse(Left(VVHError(content, url)))
  }

  /**
    * Constructions Types Allows to return
    * In Use - 0
    * Under Construction - 1
    * Planned - 3
    * Temporarily Not In Use - 4
    * Expiring Soon - 5
    */
  protected def roadLinkStatusFilter(feature: Map[String, Any]): Boolean = {
    val attributes = feature("attributes").asInstanceOf[Map[String, Any]]
    val linkStatusValue = extractAttributes(attributes).getOrElse("CONSTRUCTIONTYPE", BigInt(0)).asInstanceOf[BigInt].toInt
    ConstructionType.filteredLinkStatus.contains(ConstructionType.apply(linkStatusValue))
  }


  /**
    * Returns VVH road links in bounding box area. Municipalities are optional.
    * Used by VVHClient.fetchByRoadNumbersBoundsAndMunicipalitiesF.
    */
  protected def queryByMunicipalitiesAndBounds(bounds: BoundingRectangle, roadNumbers: Seq[(Int, Int)], municipalities: Set[Int] = Set(), includeAllPublicRoads: Boolean = false): Seq[VVHRoadlink] = {
    val roadNumberFilters = if (roadNumbers.nonEmpty || includeAllPublicRoads)
      Some(withRoadNumbersFilter(roadNumbers, includeAllPublicRoads))
    else
      None
    queryByMunicipalitiesAndBounds(bounds, municipalities, roadNumberFilters)
  }

  /**
    * Returns VVH road links by municipality.
    * Used by VVHClient.fetchByMunicipalityAndRoadNumbersF(municipality, roadNumbers) and
    * RoadLinkService.getViiteRoadLinksFromVVH(municipality, roadNumbers).
    */
  def queryByRoadNumbersAndMunicipality(municipality: Int, roadNumbers: Seq[(Int, Int)]): Seq[VVHRoadlink] = {
    val roadNumberFilters = withRoadNumbersFilter(roadNumbers, includeAllPublicRoads = true)
    val definition = layerDefinition(combineFiltersWithAnd(withMunicipalityFilter(Set(municipality)), roadNumberFilters))
    val url = serviceUrl(definition, queryParameters())
    fetchFeaturesAndLog(url)
  }

  /**
    * Returns VVH road links.
    */
  protected def queryByLinkIds[T](linkIds: Set[Long],
                                  fieldSelection: Option[String],
                                  fetchGeometry: Boolean,
                                  resultTransition: (Map[String, Any], List[List[Double]]) => T,
                                  filter: Set[Long] => String): Seq[T] = {
    val batchSize = 100
    val idGroups: List[Set[Long]] = linkIds.grouped(batchSize).toList
    idGroups.par.flatMap { ids =>
      val definition = layerDefinition(filter(ids), fieldSelection)
      val url = serviceUrl(definition, queryParameters(fetchGeometry))

      fetchVVHFeatures(url) match {
        case Right(features) => features.map { feature =>
          val attributes = extractFeatureAttributes(feature)
          val geometry = if (fetchGeometry) extractFeatureGeometry(feature) else Nil
          resultTransition(attributes, geometry)
        }
        case Left(error) =>
          logger.error("VVH error: " + error)
          throw new VVHClientException(error.toString)
      }
    }.toList
  }

  //Extract attributes methods
  protected override def extractVVHFeature(feature: Map[String, Any]): VVHRoadlink = {
    val attributes = extractFeatureAttributes(feature)
    val path = extractFeatureGeometry(feature)
    extractRoadLinkFeature(attributes, path)
  }

  protected def extractRoadLinkFeature(attributes: Map[String, Any], path: List[List[Double]]): VVHRoadlink = {
    val linkGeometry: Seq[Point] = path.map(point => {
      Point(point(0), point(1), extractMeasure(point(2)).get)
    })
    val linkGeometryForApi = Map("points" -> path.map(point => Map("x" -> point(0), "y" -> point(1), "z" -> point(2), "m" -> point(3))))
    val linkGeometryWKTForApi = Map("geometryWKT" -> ("LINESTRING ZM (" + path.map(point => point(0) + " " + point(1) + " " + point(2) + " " + point(3)).mkString(", ") + ")"))
    val linkId = attributes("LINKID").asInstanceOf[BigInt].longValue()
    val municipalityCode = attributes("MUNICIPALITYCODE").asInstanceOf[BigInt].toInt
    val mtkClass = attributes("MTKCLASS")
    val geometryLength = anyToDouble(attributes("GEOMETRYLENGTH")).getOrElse(0.0)

    val featureClassCode = if (mtkClass != null) // Complementary geometries have no MTK Class
      attributes("MTKCLASS").asInstanceOf[BigInt].intValue()
    else
      0
    val featureClass = VVHClient.featureClassCodeToFeatureClass.getOrElse(featureClassCode, FeatureClass.AllOthers)

    VVHRoadlink(linkId, municipalityCode, linkGeometry, extractAdministrativeClass(attributes),
      extractTrafficDirection(attributes), featureClass, extractModifiedAt(attributes),
      extractAttributes(attributes) ++ linkGeometryForApi ++ linkGeometryWKTForApi, extractConstructionType(attributes), linkGeomSource, geometryLength)

  }

  protected def extractLinkIdFromVVHFeature(feature: Map[String, Any]): Long = {
    extractFeatureAttributes(feature)("LINKID").asInstanceOf[BigInt].longValue()
  }

  protected def extractAdministrativeClass(attributes: Map[String, Any]): AdministrativeClass = {
    Option(attributes("ADMINCLASS").asInstanceOf[BigInt])
      .map(_.toInt)
      .map(AdministrativeClass.apply)
      .getOrElse(AdministrativeClass.Unknown)
  }

  protected def extractConstructionType(attributes: Map[String, Any]): ConstructionType = {
    Option(attributes("CONSTRUCTIONTYPE").asInstanceOf[BigInt])
      .map(_.toInt)
      .map(ConstructionType.apply)
      .getOrElse(ConstructionType.InUse)
  }

  protected def extractLinkGeomSource(attributes: Map[String, Any]): LinkGeomSource = {
    Option(attributes("LINK_SOURCE").asInstanceOf[BigInt])
      .map(_.toInt)
      .map(LinkGeomSource.apply)
      .getOrElse(LinkGeomSource.Unknown)
  }

  protected def extractTrafficDirection(attributes: Map[String, Any]): TrafficDirection = {
    Option(attributes("DIRECTIONTYPE").asInstanceOf[BigInt])
      .map(_.toInt)
      .map(vvhTrafficDirectionToTrafficDirection.getOrElse(_, TrafficDirection.UnknownDirection))
      .getOrElse(TrafficDirection.UnknownDirection)
  }

  protected def extractAttributes(attributesMap: Map[String, Any]): Map[String, Any] = {
    attributesMap.filterKeys { x =>
      Set(
        "MTKID",
        "MTKCLASS",
        "CONSTRUCTIONTYPE", //TODO Remove this attribute from here when VVHHistoryRoadLink have a different way to get the ConstructionType like VVHRoadlink
        "ROADNAME_FI",
        "ROADNAME_SM",
        "ROADNAME_SE",
        "ROADNUMBER",
        "ROADPARTNUMBER",
        "MUNICIPALITYCODE",
        "VALIDFROM",
        "GEOMETRY_EDITED_DATE",
        "CREATED_DATE",
        "LAST_EDITED_DATE",
        "SUBTYPE",
        "TRACK_CODE" // Used only with suravage and complementary in Viite
      ).contains(x)
    }.filter { case (_, value) =>
      value != null
    }
  }

  // Query filters methods
  protected def withRoadNumberFilter(roadNumbers: (Int, Int), includeAllPublicRoads: Boolean): String = {
    withLimitFilter("ROADNUMBER", roadNumbers._1, roadNumbers._2, includeAllPublicRoads)
  }

  protected def withLinkIdFilter(linkIds: Set[Long]): String = {
    withFilter("LINKID", linkIds)
  }

  protected def withMmlIdFilter(mmlIds: Set[Long]): String = {
    withFilter("MTKID", mmlIds)
  }

  protected def withMtkClassFilter(ids: Set[Long]): String = {
    withFilter("MTKCLASS", ids)
  }

  protected def withLastEditedDateFilter(lowerDate: DateTime, higherDate: DateTime): String = {
    withDateLimitFilter("LAST_EDITED_DATE", lowerDate, higherDate)
  }

  protected def withDateLimitFilter(attributeName: String, lowerDate: DateTime, higherDate: DateTime): String = {
    val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
    val since = formatter.print(lowerDate)
    val until = formatter.print(higherDate)

    s""""where":"( $attributeName >=date '$since' and $attributeName <=date '$until' )","""
  }


  protected def withRoadNumbersFilter(roadNumbers: Seq[(Int, Int)], includeAllPublicRoads: Boolean, filter: String = ""): String = {
    if (roadNumbers.isEmpty)
      return s""""where":"($filter)","""
    if (includeAllPublicRoads)
      return withRoadNumbersFilter(roadNumbers, includeAllPublicRoads = false, "ADMINCLASS = 1")
    val limit = roadNumbers.head
    val filterAdd = s"""(ROADNUMBER >= ${limit._1} and ROADNUMBER <= ${limit._2})"""
    if (filter == "")
      withRoadNumbersFilter(roadNumbers.tail, includeAllPublicRoads, filterAdd)
    else
      withRoadNumbersFilter(roadNumbers.tail, includeAllPublicRoads, s"""$filter OR $filterAdd""")
  }

  protected val vvhTrafficDirectionToTrafficDirection: Map[Int, TrafficDirection] = Map(
    0 -> TrafficDirection.BothDirections,
    1 -> TrafficDirection.TowardsDigitizing,
    2 -> TrafficDirection.AgainstDigitizing)

  /**
    * Returns VVH road links. Obtain all RoadLinks changes between two given dates.
    */
  def fetchByChangesDates(lowerDate: DateTime, higherDate: DateTime): Seq[VVHRoadlink] = {
    val definition = layerDefinition(withLastEditedDateFilter(lowerDate, higherDate))
    val url = serviceUrl(definition, queryParameters())
    fetchFeaturesAndLog(url)
  }

  /**
    * Returns VVH road link by linkid.
    */
  def fetchByLinkId(linkId: Long): Option[VVHRoadlink] = fetchByLinkIds(Set(linkId)).headOption

  /**
    * Returns VVH road links by link ids.
    */
  def fetchByLinkIds(linkIds: Set[Long]): Seq[VVHRoadlink] = {
    queryByLinkIds(linkIds, None, fetchGeometry = true, extractRoadLinkFeature, withLinkIdFilter)
  }

  def fetchByLinkIdsF(linkIds: Set[Long]): Future[Seq[VVHRoadlink]] = {
    Future(fetchByLinkIds(linkIds))
  }

  /**
    * Returns VVH road link by mml id.
    * Used by RoadLinkService.getRoadLinkMiddlePointByMmlId
    */
  def fetchByMmlId(mmlId: Long): Option[VVHRoadlink] = fetchByMmlIds(Set(mmlId)).headOption

  /**
    * Returns VVH road links by mml ids.
    * Used by VVHClient.fetchByMmlId, LinkIdImporter.updateTable and AssetDataImporter.importRoadAddressData.
    */
  def fetchByMmlIds(mmlIds: Set[Long]): Seq[VVHRoadlink] = {
    queryByLinkIds(mmlIds, None, fetchGeometry = true, extractRoadLinkFeature, withMmlIdFilter)
  }

  def fetchByMunicipality(municipality: Int): Seq[VVHRoadlink] = {
    queryByMunicipality(municipality)
  }

  def fetchByMunicipalityF(municipality: Int): Future[Seq[VVHRoadlink]] = {
    Future(queryByMunicipality(municipality))
  }

  /**
    * Returns VVH road links. Uses Scala Future for concurrent operations.
    */
  def fetchByMunicipalitiesAndBounds(bounds: BoundingRectangle, municipalities: Set[Int]): Seq[VVHRoadlink] = {
    queryByMunicipalitiesAndBounds(bounds, municipalities)
  }

  def fetchByBounds(bounds: BoundingRectangle): Seq[VVHRoadlink] = {
    queryByMunicipalitiesAndBounds(bounds, Set[Int]())
  }

  /**
    * Returns VVH road links. Uses Scala Future for concurrent operations.
    * Used by RoadLinkService.getRoadLinksAndChangesFromVVH(bounds, municipalities),
    * RoadLinkService.getViiteRoadLinksAndChangesFromVVH(bounds, roadNumbers, municipalities, everything, publicRoads).
    */
  def fetchByBoundsAndMunicipalitiesF(bounds: BoundingRectangle, municipalities: Set[Int]): Future[Seq[VVHRoadlink]] = {
    Future(queryByMunicipalitiesAndBounds(bounds, municipalities))
  }

  /**
    * Returns VVH road links. Uses Scala Future for concurrent operations.
    * Used by RoadLinkService.getRoadLinksAndChangesFromVVH(bounds, municipalities).
    */
  def fetchByRoadNumbersBoundsAndMunicipalitiesF(bounds: BoundingRectangle, municipalities: Set[Int], roadNumbers: Seq[(Int, Int)],
                                                 includeAllPublicRoads: Boolean = false): Future[Seq[VVHRoadlink]] = {
    Future(queryByMunicipalitiesAndBounds(bounds, roadNumbers, municipalities, includeAllPublicRoads))
  }

  /**
    * Returns a sequence of VVH Road Links. Uses Scala Future for concurrent operations.
    * Used by RoadLinkService.getViiteCurrentAndComplementaryRoadLinksFromVVH(municipality, roadNumbers).
    */
  def fetchByMunicipalityAndRoadNumbersF(municipality: Int, roadNumbers: Seq[(Int, Int)]): Future[Seq[VVHRoadlink]] = {
    Future(queryByRoadNumbersAndMunicipality(municipality, roadNumbers))
  }

  def fetchByMunicipalityAndRoadNumbers(municipality: Int, roadNumbers: Seq[(Int, Int)]): Seq[VVHRoadlink] = {
    queryByRoadNumbersAndMunicipality(municipality, roadNumbers)
  }

  /**
    * Returns VVH road links.
    */
  def fetchVVHRoadlinks[T](linkIds: Set[Long],
                           fieldSelection: Option[String],
                           fetchGeometry: Boolean,
                           resultTransition: (Map[String, Any], List[List[Double]]) => T): Seq[T] =
    queryByLinkIds(linkIds, fieldSelection, fetchGeometry, resultTransition, withLinkIdFilter)

}

class VVHChangeInfoClient(vvhRestApiEndPoint: String) extends VVHClientOperations {
  override type VVHType = ChangeInfo

  protected override val restApiEndPoint: String = vvhRestApiEndPoint
  protected override val serviceName = "Roadlink_ChangeInfo"
  protected override val linkGeomSource: LinkGeomSource.Unknown.type = LinkGeomSource.Unknown
  protected override val disableGeometry = true

  protected override def defaultOutFields(): String = {
    "OLD_ID,NEW_ID,MTKID,CHANGETYPE,OLD_START,OLD_END,NEW_START,NEW_END,CREATED_DATE,CONSTRUCTIONTYPE"
  }

  protected override def mapFields(content: Map[String, Any], url: String): Either[VVHError, List[Map[String, Any]]] = {
    val optionalLayers = content.get("layers").map(_.asInstanceOf[List[Map[String, Any]]])
    val optionalFeatureLayer = optionalLayers.flatMap { layers => layers.find { layer => layer.contains("features") } }
    val optionalFeatures = optionalFeatureLayer.flatMap { featureLayer => featureLayer.get("features").map(_.asInstanceOf[List[Map[String, Any]]]) }
    optionalFeatures.map(Right(_)).getOrElse(Left(VVHError(content, url)))
  }

  protected override def extractVVHFeature(feature: Map[String, Any]): ChangeInfo = {
    val attributes = extractFeatureAttributes(feature)

    val oldId = Option(attributes("OLD_ID").asInstanceOf[BigInt]).map(_.longValue())
    val newId = Option(attributes("NEW_ID").asInstanceOf[BigInt]).map(_.longValue())
    val mmlId = attributes("MTKID").asInstanceOf[BigInt].longValue()
    val changeType = attributes("CHANGETYPE").asInstanceOf[BigInt].intValue()
    val vvhTimeStamp = Option(attributes("CREATED_DATE").asInstanceOf[BigInt]).map(_.longValue()).getOrElse(0L)
    val oldStartMeasure = extractMeasure(attributes("OLD_START"))
    val oldEndMeasure = extractMeasure(attributes("OLD_END"))
    val newStartMeasure = extractMeasure(attributes("NEW_START"))
    val newEndMeasure = extractMeasure(attributes("NEW_END"))

    ChangeInfo(oldId, newId, mmlId, ChangeType.apply(changeType), oldStartMeasure, oldEndMeasure, newStartMeasure, newEndMeasure, vvhTimeStamp)
  }

  def fetchByBoundsAndMunicipalities(bounds: BoundingRectangle, municipalities: Set[Int]): Seq[ChangeInfo] = {
    queryByMunicipalitiesAndBounds(bounds, municipalities)
  }

  def fetchByMunicipality(municipality: Int): Seq[ChangeInfo] = {
    queryByMunicipality(municipality)
  }

  def fetchByBoundsAndMunicipalitiesF(bounds: BoundingRectangle, municipalities: Set[Int]): Future[Seq[ChangeInfo]] = {
    Future(queryByMunicipalitiesAndBounds(bounds, municipalities))
  }

  def fetchByMunicipalityF(municipality: Int): Future[Seq[ChangeInfo]] = {
    Future(queryByMunicipality(municipality))
  }

  def fetchByLinkIdsF(linkIds: Set[Long]): Future[Seq[ChangeInfo]] = {
    Future(fetchByLinkIds(linkIds))
  }

  /**
    * Fetch change information where given link id is in the old_id list (source)
    *
    * @param linkIds Link ids to check as sources
    * @return ChangeInfo for given links
    */
  def fetchByLinkIds(linkIds: Set[Long]): Seq[ChangeInfo] = {
    queryByLinkIds(linkIds, "OLD_ID")
  }

  /**
    * Fetch change information where given link id is in the new_id list (source)
    *
    * @param linkIds Link ids to check as sources
    * @return ChangeInfo for given links
    */
  def fetchByNewLinkIds(linkIds: Set[Long]): Seq[ChangeInfo] = {
    queryByLinkIds(linkIds, "NEW_ID")
  }

  protected def queryByLinkIds(linkIds: Set[Long], field: String): Seq[ChangeInfo] = {
    val batchSize = 1000
    val idGroups: List[Set[Long]] = linkIds.grouped(batchSize).toList
    idGroups.par.flatMap { ids =>
      val definition = layerDefinition(withFilter(field, ids))
      val url = serviceUrl(definition, queryParameters(false))
      fetchFeaturesAndLog(url)
    }.toList
  }
}

class VVHComplementaryClient(vvhRestApiEndPoint: String) extends VVHRoadLinkClient(vvhRestApiEndPoint) {

  protected override val restApiEndPoint: String = vvhRestApiEndPoint
  protected override val serviceName = "Roadlink_complimentary"
  protected override val linkGeomSource: LinkGeomSource = LinkGeomSource.ComplementaryLinkInterface
  protected override val disableGeometry = false

  override def defaultOutFields(): String = {
    "MTKID,LINKID,MUNICIPALITYCODE,MTKCLASS,ADMINCLASS,DIRECTIONTYPE,ROADNAME_FI,ROADNAME_SM,ROADNAME_SE,LAST_EDITED_DATE,ROADNUMBER,ROADPARTNUMBER,VALIDFROM,GEOMETRY_EDITED_DATE,CREATED_DATE,SUBTYPE,CONSTRUCTIONTYPE,GEOMETRYLENGTH,TRACK_CODE"
  }

  private def createFormParams(complementaryFeatures: Map[String, Any]): util.ArrayList[NameValuePair] = {
    val featuresValue = Serialization.write(Seq(Map("attributes" -> complementaryFeatures)))
    // Print JSON sent to VVH for testing purposes
    logger.info("complementaryFeatures to JSON: %s".format(featuresValue))

    val nvps = new util.ArrayList[NameValuePair]()
    nvps.add(new BasicNameValuePair("features", featuresValue))
    nvps.add(new BasicNameValuePair("gdbVersion", ""))
    nvps.add(new BasicNameValuePair("rollbackOnFailure", "true"))
    nvps.add(new BasicNameValuePair("f", "json"))

    nvps
  }

  def fetchWalkwaysByBoundsAndMunicipalitiesF(bounds: BoundingRectangle, municipalities: Set[Int]): Future[Seq[VVHRoadlink]] = {
    Future(queryByMunicipalitiesAndBounds(bounds, municipalities, Some(withMtkClassFilter(Set(12314)))))
  }

  def fetchComplementaryByMunicipalitiesF(municipality: Int): Future[Seq[VVHRoadlink]] =
    Future(queryByMunicipality(municipality))

  def updateVVHFeatures(complementaryFeatures: Map[String, Any]): Either[List[Map[String, Any]], VVHError] = {
    val url = vvhRestApiEndPoint + serviceName + "/FeatureServer/0/updateFeatures"
    time(logger, s"Update VVH features with url: '$url'") {
      val request = new HttpPost(url)
      request.setEntity(new UrlEncodedFormEntity(createFormParams(complementaryFeatures), "utf-8"))
      val client = HttpClientBuilder.create().build()
      try {
        val response = client.execute(request)
        try {
          val content: Map[String, Seq[Map[String, Any]]] = parse(StreamInput(response.getEntity.getContent)).values.asInstanceOf[Map[String, Seq[Map[String, Any]]]]
          content.getOrElse("updateResults", None) match {
            case None =>
              content.get("error").head.asInstanceOf[Map[String, Any]].getOrElse("details", None) match {
                case None => Right(VVHError(Map("error" -> "Error Without Details "), url))
                case value => Right(VVHError(Map("error details" -> value), url))
              }
            case _ =>
              content("updateResults").map(_.getOrElse("success", None)).head match {
                case None => Right(VVHError(Map("error" -> "Update status not available in JSON Response"), url))
                case true => Left(List(content))
                case false =>
                  content("updateResults").map(_.getOrElse("error", None)).head.asInstanceOf[Map[String, Any]].getOrElse("description", None) match {
                    case None => Right(VVHError(Map("error" -> "Error Without Information"), url))
                    case value => Right(VVHError(Map("error" -> value), url))
                  }
              }
          }
        } catch {
          case e: Exception => Right(VVHError(Map("error" -> e.getMessage), url))
        } finally {
          response.close()
          if (response.getStatusLine.getStatusCode >= 300) {
           return  Right(VVHError(Map(("VVH FETCH failure", "VVH response code was <300 (unsuccessful)")), url))
          }
        }
      } catch {
        case ioe: IOException => Right(VVHError(Map(("VVH FETCH failure", "IO Exception during VVH fetch. Check connection to VVH")), url))
      }
    }
  }
}

class VVHHistoryClient(vvhRestApiEndPoint: String) extends VVHRoadLinkClient(vvhRestApiEndPoint) {

  protected override val restApiEndPoint: String = vvhRestApiEndPoint
  protected override val serviceName = "Roadlink_data_history"
  protected override val linkGeomSource: LinkGeomSource.HistoryLinkInterface.type = LinkGeomSource.HistoryLinkInterface
  protected override val disableGeometry = false

  protected override def defaultOutFields(): String = {
    "MTKID,LINKID,MUNICIPALITYCODE,MTKCLASS,ADMINCLASS,DIRECTIONTYPE,CONSTRUCTIONTYPE,ROADNAME_FI,ROADNAME_SM,ROADNAME_SE,LAST_EDITED_DATE,ROADNUMBER,ROADPARTNUMBER,VALIDFROM,GEOMETRY_EDITED_DATE,END_DATE,CREATED_DATE,CONSTRUCTIONTYPE,GEOMETRYLENGTH"
  }

  protected override def mapFields(content: Map[String, Any], url: String): Either[VVHError, List[Map[String, Any]]] = {
    val optionalFeatures = if (content.contains("layers")) {
      val optionalLayers = content.get("layers").map(_.asInstanceOf[List[Map[String, Any]]])
      val optionalFeatureLayer = optionalLayers.flatMap { layers => layers.find { layer => layer.contains("features") } }
      optionalFeatureLayer.flatMap { featureLayer => featureLayer.get("features").map(_.asInstanceOf[List[Map[String, Any]]]) }
    }
    else {
      content.get("features").map(_.asInstanceOf[List[Map[String, Any]]])
    }
    optionalFeatures.map(Right(_)).getOrElse(Left(VVHError(content, url)))
  }

  protected def extractVVHHistoricFeature(feature: Map[String, Any]): VVHHistoryRoadLink = {
    val attributes = extractFeatureAttributes(feature)
    val path = extractFeatureGeometry(feature)
    val linkGeometry: Seq[Point] = path.map(point => {
      Point(point(0), point(1), extractMeasure(point(2)).get)
    })
    val municipalityCode = attributes("MUNICIPALITYCODE").asInstanceOf[BigInt].toInt
    val linkGeometryForApi = Map("points" -> path.map(point => Map("x" -> point(0), "y" -> point(1), "z" -> point(2), "m" -> point(3))))
    val linkGeometryWKTForApi = Map("geometryWKT" -> ("LINESTRING ZM (" + path.map(point => point(0) + " " + point(1) + " " + point(2) + " " + point(3)).mkString(", ") + ")"))
    val linkId = attributes("LINKID").asInstanceOf[BigInt].longValue()
    val createdDate = attributes("CREATED_DATE").asInstanceOf[BigInt].longValue()
    val endTime = attributes("END_DATE").asInstanceOf[BigInt].longValue()
    val mtkClass = attributes("MTKCLASS")
    val featureClassCode = if (mtkClass != null) // Complementary geometries have no MTK Class
      attributes("MTKCLASS").asInstanceOf[BigInt].intValue()
    else
      0
    val featureClass = VVHClient.featureClassCodeToFeatureClass.getOrElse(featureClassCode, FeatureClass.AllOthers)

    VVHHistoryRoadLink(linkId, municipalityCode, linkGeometry, extractAdministrativeClass(attributes),
      extractTrafficDirection(attributes), featureClass, createdDate, endTime, extractAttributes(attributes) ++ linkGeometryForApi ++ linkGeometryWKTForApi)
  }

  /**
    * Returns VVH road link history data in bounding box area. Municipalities are optional.
    * Used by VVHClient.fetchVVHRoadlinksF, RoadLinkService.getVVHRoadLinks(bounds, municipalities), RoadLinkService.getVVHRoadLinks(bounds),
    * PointAssetService.getByBoundingBox and ServicePointImporter.importServicePoints.
    */
  def fetchVVHRoadLinkByLinkIds(linkIds: Set[Long] = Set()): Seq[VVHHistoryRoadLink] = {
    if (linkIds.isEmpty)
      Nil
    else {
      val batchSize = 1000
      val idGroups: List[Set[Long]] = linkIds.grouped(batchSize).toList
      idGroups.par.flatMap { ids =>
        val definition = layerDefinition(withLinkIdFilter(ids))
        val url = serviceUrl(definition, queryParameters())

        fetchVVHFeatures(url) match {
          case Right(features) => features.map(extractVVHHistoricFeature)
          case Left(error) =>
            logger.error("VVH error: " + error)
            throw new VVHClientException(error.toString)
        }
      }.toList
    }
  }

  def fetchVVHRoadLinkByLinkIdsF(linkIds: Set[Long] = Set()): Future[Seq[VVHHistoryRoadLink]] = {
    Future(fetchVVHRoadLinkByLinkIds(linkIds))
  }
}
