package fi.liikennevirasto.digiroad2.util

import java.io.File

import fi.liikennevirasto.digiroad2.client.kmtk.ChangeInfo
import fi.liikennevirasto.digiroad2.linearasset.RoadLink

/**
  * Created by venholat on 2.6.2016.
  */
trait RoadLinkSerializer {

  def readCachedGeometry(file: File): Seq[RoadLink]

  def readCachedChanges(file: File): Seq[ChangeInfo]

  def writeCache(file: File, changes: Seq[Object]): Boolean
}