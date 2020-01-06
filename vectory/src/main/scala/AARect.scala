package vectory

import spire.implicits._

object AARect {
  def fromCenter(center: Vec2d, size: Vec2d) = {
    val halfSize = size * 0.5
    val minCorner = center - halfSize
    val maxCorner = center + halfSize
    new AARect(minCorner, maxCorner)
  }
}

final case class AARect(minCorner: Vec2d, maxCorner: Vec2d) extends ConvexPolygonLike {
  override def aabb = this

  override def includes(v: Vec2d): Boolean = v.x > minCorner.x && v.y > minCorner.y && v.x < maxCorner.x && v.y < maxCorner.y

  @inline def size = maxCorner - minCorner
  @inline def width = size.x
  @inline def height = size.y
  @inline def center = (maxCorner + minCorner) * 0.5
  lazy val verticesCCW = Vec2Array(
    minCorner,
    minCorner + Vec2(width, 0),
    maxCorner,
    minCorner + Vec2(0, height)
  )

  //TODO: optimized AARect vs AARect collision detection and repsonse only looking at two axes
  //TODO: optimized Circle vs AARect collision detection and repsonse

  override def intersects(circle: Circle) = Algorithms.intersect(circle, this)
}
