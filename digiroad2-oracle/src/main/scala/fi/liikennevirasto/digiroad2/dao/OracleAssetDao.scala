package fi.liikennevirasto.digiroad2.dao

import com.github.tototoshi.slick.MySQLJodaSupport._
import fi.liikennevirasto.digiroad2.oracle.MassQuery
import org.joda.time.DateTime
import slick.driver.JdbcDriver.backend.Database.dynamicSession
import slick.jdbc.StaticQuery.interpolation
import slick.jdbc.{StaticQuery => Q}


class OracleAssetDao {

  def getLastExecutionDate(typeId: Int, createdBy: String): Option[DateTime] = {

    sql""" select MAX( case when a.modified_date is null then MAX(a.created_date) else MAX(a.modified_date) end ) as lastExecution
           from asset a
           where a.created_by = $createdBy and ( a.modified_by = $createdBy or a.modified_by is null) and a.asset_type_id = $typeId
           group by a.modified_date, a.created_date""".as[DateTime].firstOption

  }

  def getMunicipalityById(id: Long): Seq[Long] = {
    val municipalityCode = sql"""select id from municipality where id = $id """.as[Long].list
    municipalityCode
  }

  def getGeometryType(typeId: Int): String = {
    val geometryType = sql""" select GEOMETRY_TYPE from asset_type where id = $typeId""".as[String].firstOption.get
    geometryType
  }

  /**
    * When invoked will expire assets by Id.
    * It is required that the invoker takes care of the transaction.
    *
    * @param id Represets the id of the asset
    */
  def expireAssetsById (id: Long): Unit = {
    sqlu"update asset set valid_to = sysdate - 1/86400 where id = $id".execute
  }

  /**
    * When invoked will expire assets by type id and link ids.
    * It is required that the invoker takes care of the transaction.
    *
    * @param typeId Represets the id of the asset
    * @param linkIds Represets the link id of the road
    */
  def expireAssetByTypeAndLinkId(typeId: Long, linkIds: Seq[Long]): Unit = {
    MassQuery.withIds(linkIds.toSet) { idTableName =>
      sqlu"""
         update asset set valid_to = sysdate - 1/86400 where id in (
          select a.id
          from asset a
          join asset_link al on al.asset_id = a.id
          join lrm_position lrm on lrm.id = al.position_id
          join  #$idTableName i on i.id = lrm.link_id
          where a.asset_type_id = $typeId AND (a.valid_to IS NULL OR a.valid_to > SYSDATE ) AND a.floating = 0
         )
      """.execute
    }
  }

  /**
    * Returns the municipality code of a Asset by it's Id
    *
    * @param assetId The Id of the Asset
    * @return Type: Int - The Municipality Code
    */
  def getAssetMunicipalityCodeById(assetId: Int): Int = {
    val municipalityCode = sql"""Select municipality_code From asset Where id= $assetId""".as[Int].firstOption.get
    municipalityCode
  }

  def getAssetTypeId(ids: Seq[Long]): Seq[(Long, Int)] = {
    sql"""select ID, ASSET_TYPE_ID from ASSET where ID in (#${ids.mkString(",")})""".as[(Long, Int)].list
  }

  def getVerifiableAssetTypes: Seq[String] = {
    val assetTypes = sql"""select name from asset_type where verifiable = 1""".as[(String)].list
    assetTypes
  }
}