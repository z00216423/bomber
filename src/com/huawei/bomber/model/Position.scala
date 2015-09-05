package com.huawei.bomber.model

/**
 * Created by frank on 8/30/15.
 */

case class Position(val x: Int, val y: Int){

  def canEqual(other: Any): Boolean = other.isInstanceOf[Position]

  override def equals(other: Any): Boolean = other match {
    case that: Position =>
      (that canEqual this) &&
        x == that.x &&
        y == that.y
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(x, y)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  override def toString: String = s"($x, $y)"
}
