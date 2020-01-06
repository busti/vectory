package vectory

import annotation.meta.field
import flatland._

import spire.algebra._
import spire.implicits._

final case class RotatedRect(center: Vec2d, size: Vec2d, angle: Double) extends ConvexPolygonLike {
  implicit val $r: CRing[Double] = implicitly[CRing[Double]]

  import Math.{ sin, cos }

  @inline def width = size.x
  @inline def height = size.y

  lazy val toRight = Vec2(cos(angle), sin(angle)) * (width * 0.5)
  lazy val toTop = Vec2(-sin(angle), cos(angle)) * (height * 0.5)

  lazy val minCorner = center - toRight - toTop
  lazy val maxCorner = center + toRight + toTop

  lazy val verticesCCW = Vec2Array(
    minCorner,
    center + toRight - toTop,
    maxCorner,
    center - toRight + toTop
  )
}

