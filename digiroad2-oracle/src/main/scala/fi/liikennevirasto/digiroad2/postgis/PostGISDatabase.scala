package fi.liikennevirasto.digiroad2.postgis

import java.sql.Date

import javax.sql.DataSource
import com.jolbox.bonecp.{BoneCPConfig, BoneCPDataSource}
import fi.liikennevirasto.digiroad2.asset.BoundingRectangle
import org.joda.time.LocalDate
import slick.driver.JdbcDriver.backend.Database
import fi.liikennevirasto.digiroad2.util.ViiteProperties
import fi.liikennevirasto.digiroad2.{GeometryUtils, Point}
import org.postgis.PGgeometry
import org.postgresql.util.PGobject

object PostGISDatabase {
  lazy val ds: DataSource = initDataSource

  private val transactionOpen = new ThreadLocal[Boolean] {
    override def initialValue(): Boolean = { false }
  }

  /** Opens a transaction session, for db operations to be processed as an atomic set.
    * The transaction, and session are closed at return.
    *  @throws IllegalThreadStateException when a session is already open. */
  def withDynTransaction[T](f: => T): T = {
    if (transactionOpen.get())
      throw new IllegalThreadStateException("Attempted to open nested transaction")
    else {
      try {
        transactionOpen.set(true)
        Database.forDataSource(PostGISDatabase.ds).withDynTransaction {
          setSessionLanguage()
          f
        }
      } finally {
        transactionOpen.set(false)
      }
    }
  }

  /** Opens a transaction session for non-atomic db operations, or just runs the given function,
    * if a transaction is already open.
    * If the transaction, and session were opened, they are closed at return. */
  def withDynTransactionNewOrExisting[T](f: => T): T = {
    if (transactionOpen.get()) { // TODO Fix error, where f is run without transaction, if withDynSession called before this, instead of withDynTransaction
      f
    } else {
      try {
        transactionOpen.set(true)
        Database.forDataSource(PostGISDatabase.ds).withDynTransaction {
          setSessionLanguage()
          f
        }
      } finally {
        transactionOpen.set(false)
      }
    }
  }

  /** Opens a session for non-atomic db operations.
    * Session is closed at return.
    *  If you need atomicity, use {@link withDynTransaction} instead.
    *  @throws IllegalThreadStateException, if a session is already open. */
  def withDynSession[T](f: => T): T = {
    if (transactionOpen.get())
      throw new IllegalThreadStateException("Attempted to open nested session")
    else {
      try {
        transactionOpen.set(true)
        Database.forDataSource(PostGISDatabase.ds).withDynSession {
          setSessionLanguage()
          f
        }
      } finally {
        transactionOpen.set(false)
      }
    }
  }

  def isWithinSession: Boolean = {
    transactionOpen.get()
  }

  def setSessionLanguage() {
    //sqlu"""alter session set nls_language = 'american'""".execute
  }

  def jodaToSqlDate(jodaDate: LocalDate): Date = {
    new Date(jodaDate.toDate.getTime)
  }

  def initDataSource: DataSource = {
    Class.forName("org.postgresql.Driver")
    val cfg = new BoneCPConfig(ViiteProperties.bonecpProperties)
    new BoneCPDataSource(cfg)
  }

  def boundingBoxFilter(bounds: BoundingRectangle, geometryColumn: String): String = {
    val leftBottomX = bounds.leftBottom.x
    val leftBottomY = bounds.leftBottom.y
    val rightTopX = bounds.rightTop.x
    val rightTopY = bounds.rightTop.y
    s"""
      $geometryColumn && ST_MakeEnvelope($leftBottomX,
                                         $leftBottomY,
                                         $rightTopX,
                                         $rightTopY,
                                         3067)
    """
  }

  def createJGeometry(points: Seq[Point]): String = {
    if (points.nonEmpty) {
      s"""LINESTRING(${points.map(p => s"""${p.x} ${p.y} ${p.z}""").mkString(", ")})"""
    } else {
      "LINESTRING EMPTY"
    }
  }

  def createXYZMGeometry(points: Seq[(Point, Double)]): String = {
    if (points.nonEmpty) {
      s"""LINESTRING(${points.map(p => s"""${p._1.x} ${p._1.y} ${p._1.z} ${p._2}""").mkString(", ")})"""
    } else {
      "LINESTRING EMPTY"
    }
  }

  def createPointGeometry(point: Point): String = {
    s"""POINT(${point.x} ${point.y})"""
  }

  def createRoundedPointGeometry(point: Point): String = {
    s"""POINT(${GeometryUtils.scaleToThreeDigits(point.x)} ${GeometryUtils.scaleToThreeDigits(point.y)})"""
  }

  // TODO Maybe this should be optimized
  def loadJGeometryToGeometry(geometry: Option[Object]): Seq[Point] = {
    if (geometry.nonEmpty) {
      val pgObject = geometry.get.asInstanceOf[PGobject]
      val geom = PGgeometry.geomFromString(pgObject.getValue)
      val n = geom.numPoints()
      var points: Seq[Point] = Seq()
      for (i <- 1 to n) {
        val point = geom.getPoint(i - 1)
        points = points :+ Point(point.x, point.y, point.z)
      }
      points
    } else {
      Seq()
    }
  }

}