package com.huawei.bomber

import com.huawei.bomber.model._


/**
 * Created by frank on 8/30/15.
 */
object App  {


  def main(args: Array[String]) {

    val walls: Array[Position] = Array(Position(1, 1), Position(4, 3))
    val boxes: Array[Position] = Array(Position(2, 2), Position(3, 3))
    val bombs: Array[Bomb] = Array(Bomb(1, 3, 1, 1), Bomb(2, 2, 4, 1) , Bomb(1, 0,4,2), Bomb(2, 2, 1, 3))
    MapAnalyzer.initMap(5,5, walls, boxes, bombs)

    MapAnalyzer.printMap()

    val ps = MapAnalyzer.getDangerPositions()
    println(s"${ps.size} dangerious positions: ")
    println(ps.mkString(","))

    val copyMap = MapAnalyzer.getMap().clone()
    ps.foreach(p => copyMap(p.x)(p.y) = 1)
    MapAnalyzer.printMap(copyMap)

  }

}



