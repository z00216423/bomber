package com.huawei.bomber

import com.huawei.bomber.model.{Bomb, Position}
import org.junit.Test

/**
 * Created by frank on 8/30/15.
 */
class TestMapAnalyzer {

  @Test
  def testDangerPosition(): Unit = {

    init

    MapAnalyzer.printMap()

    val ps = MapAnalyzer.getDangerPositions()

    println(s"${ps.size} dangerious positions: ")
    println(ps.mkString(","))

    assert(ps.size == 16)
    val expect = List(Position(0, 2),
      Position(0, 3),
      Position(0, 4),
      Position(1, 4),
      Position(2, 0),
      Position(2, 1),
      Position(2, 2),
      Position(2, 3),
      Position(2, 4),
      Position(3, 0),
      Position(3, 1),
      Position(3, 2),
      Position(3, 3),
      Position(3, 4),
      Position(4, 1),
      Position(4, 4))

    expect.foreach(ex => assert(ps.contains(ex)))
  }

  def init: Unit = {
    val walls: Array[Position] = Array(Position(1, 1), Position(4, 3))
    val boxes: Array[Position] = Array(Position(2, 2), Position(3, 3))
    val bombs: Array[Bomb] = Array(Bomb(1, 3, 1, 1), Bomb(2, 2, 4, 1), Bomb(1, 0, 4, 2), Bomb(2, 2, 1, 3))
    MapAnalyzer.initMap(5, 5, walls, boxes, bombs)
  }

  @Test
  def testNextDirection(): Unit = {

    init


    MapAnalyzer.printMap()

    val directs = MapAnalyzer.nextDirections(Position(1,2))
    assert(directs.size == 2)

    val safeDirects = MapAnalyzer.nextSafeDirections(Position(1,2))
    assert(safeDirects.size == 1)
  }

  @Test
  def testPossiblePath: Unit = {

    init

    MapAnalyzer.printMap()

    println("not boost")
    val start: Position = Position(1, 3)
    MapAnalyzer.possiblePath(start, false).foreach(p => println(p.getFootprint()))

    println("boost")
    MapAnalyzer.possiblePath(start, true).foreach(p => println(p.getFootprint()))


    val possiblePath = MapAnalyzer.possiblePath(start, true)
    val dangerPosArr = MapAnalyzer.getDangerPositions()

    println("safe path")
    possiblePath.filter(path => !dangerPosArr.contains(path.getDestination)).foreach(p => println(p.getFootprint))
  }


  
  


}
