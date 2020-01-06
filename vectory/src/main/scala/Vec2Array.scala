package vectory

import annotation.meta.field

import flatland._

object Vec2Array {
  @inline def create(n: Int) = new Vec2Array(InterleavedArrayDouble.create(n))
  def apply(vecs: Vec2d*) = {
    val n = vecs.length
    val v2a = create(n)
    var i = 0
    while (i < n) {
      v2a(i) = vecs(i)
      i += 1
    }
    v2a
  }
  implicit def toIndexdSeq(v2a: Vec2Array): IndexedSeq[Vec2d] = new IndexedSeq[Vec2d] {
    def apply(idx: Int): Vec2d = v2a(idx)
    def length: Int = v2a.length
  }
}
@inline final class Vec2Array(val interleaved: InterleavedArrayDouble) {
  @inline def length = interleaved.elementCount
  @inline def apply(i: Int) = new Vec2(interleaved.x(i), interleaved.y(i))
  @inline def update(i: Int, newVec: Vec2d) = {
    interleaved.updatex(i, newVec.x)
    interleaved.updatey(i, newVec.y)
  }
}

