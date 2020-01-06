package vectory

import scala.{specialized => sp}
import spire.algebra._
import spire.implicits._

@inline final case class Vec2[@sp(Float, Double) T](x: T, y: T) {
  @inline def width: T = x
  @inline def height: T = y

  @inline def unary_-(implicit r: CRing[T]): Vec2[T] = Vec2(-x, -y)
  @inline def abs(implicit s: Signed[T]): Vec2[T] = Vec2(x.abs, y.abs)

  @inline def +(that: Vec2[T])(implicit r: CRing[T]): Vec2[T] = Vec2(this.x + that.x, this.y + that.y)
  @inline def +(that: T)(implicit r: CRing[T]): Vec2[T] = Vec2(this.x + that, this.y + that)
  @inline def -(that: Vec2[T])(implicit r: CRing[T]): Vec2[T] = Vec2(this.x - that.x, this.y - that.y)
  @inline def -(that: T)(implicit r: CRing[T]): Vec2[T] = Vec2(this.x - that, this.y - that)
  @inline def *(that: T)(implicit r: CRing[T]): Vec2[T] = Vec2(this.x * that, this.y * that)
  @inline def /(that: T)(implicit f: Field[T]): Vec2[T] = Vec2(this.x / that, this.y / that)
  @inline def dot(that: Vec2[T])(implicit r: CRing[T]): T = this.x * that.x + this.y * that.y
  @inline def cross(that: Vec2[T])(implicit r: CRing[T]): T = this.x * that.y - this.y * that.x

  @inline def lengthSq(implicit r: CRing[T]): T = x * x + y * y
  @inline def length(implicit r: CRing[T], n: NRoot[T]): T = lengthSq.sqrt
  @inline def normalized(implicit r: CRing[T], f: Field[T], n: NRoot[T]): Vec2[T] = this / length
  @inline def area(implicit r: CRing[T]): T = x * y
  @inline def normal(implicit r: CRing[T]): Vec2[T] = Vec2(y, -x)

  @inline def angle(implicit t: Trig[T]): T = t.atan2(y, x)

  @inline def toTuple: (T, T) = (x, y)
  @inline def toArray: Array[T] = {
    val a = new Array[T](2)
    a(0) = x
    a(1) = y
    a
  }
}

object Vec2 {
  @inline def apply[@sp(Float, Double) T](tuple: (T, T)): Vec2[T] = new Vec2(tuple._1, tuple._2)
  @inline def apply[@sp(Float, Double) T](x: T): Vec2[T] = new Vec2(x, x)
  @inline def apply[@sp(Float, Double) T](v: { def x: T; def y: T }): Vec2[T] = new Vec2(v.x, v.y)
  @inline def dim[@sp(Float, Double) T](v: { def width: T; def height: T }): Vec2[T] = new Vec2(v.width, v.height)

  @inline def zero[@sp(Float, Double) T](implicit r: Rig[T]): Vec2[T] = new Vec2(r.zero, r.zero)
  @inline def unitX[@sp(Float, Double) T](implicit r: Rig[T]): Vec2[T] = new Vec2(r.one, r.zero)
  @inline def unitY[@sp(Float, Double) T](implicit r: Rig[T]): Vec2[T] = new Vec2(r.zero, r.one)
  @inline def unit[@sp(Float, Double) T](angle: T)(implicit t: Trig[T]): Vec2[T] = Vec2(t.cos(angle), t.sin(angle))

  @inline def dot[@sp(Float, Double) T: CRing](x1: T, y1: T, x2: T, y2: T) = x1 * x2 + y1 * y2
  @inline def lengthSq[@sp(Float, Double) T: CRing](x: T, y: T) = x * x + y * y
  @inline def length[@sp(Float, Double) T: CRing: NRoot](x: T, y: T) = lengthSq(x, y).sqrt
  @inline def normalize[@sp(Float, Double) T: Field](length: T, component: T) = component / length
}
