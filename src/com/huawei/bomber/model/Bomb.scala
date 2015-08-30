package com.huawei.bomber.model

import com.huawei.bomber.common.Constant

/**
 * Created by frank on 8/30/15.
 */
case class Bomb(val teamId: Int, val x: Int, val y: Int, var countDown: Int) {

  def this(teamId: Int, x: Int, y: Int) = {
    this(teamId, x, y, Constant.MAP_BOMB_COUNT_3)
  }

  def this(teamId: Int, p: Position) = this(teamId, p.x, p.y)

  override def toString = s"Bomb($teamId, $x, $y, $countDown)"
}
