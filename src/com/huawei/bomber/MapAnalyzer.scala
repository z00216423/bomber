package com.huawei.bomber

import com.huawei.bomber.common.{Constant, Utils}
import com.huawei.bomber.model.{BomberPath, Bomb, DirectionEnum, Position}

/**
 * Created by frank on 8/30/15.
 */
object MapAnalyzer {

  private var map: Array[Array[Int]] = null

  private[this] var width = 0
  private[this] var height = 0


  def initMap(width: Int, height: Int): Unit = {
    this.width = width
    this.height = height
    map = Array.ofDim[Int](width, height)
  }

  def initMap(width: Int, height: Int, walls: Array[Position]): Unit = {
    initMap(width, height)
    walls.foreach(validatePosition)
    walls.foreach(p => map(p.x)(p.y) = Constant.MAP_WALL)
  }

  def initMap(width: Int, height: Int, walls: Array[Position], boxes: Array[Position]): Unit = {
    initMap(width, height, walls)
    boxes.foreach(validatePosition)
    boxes.foreach(box => map(box.x)(box.y) = Constant.MAP_BOX)
  }

  def initMap(width: Int, height: Int, walls: Array[Position], boxes: Array[Position], bombs: Array[Bomb]): Unit = {
    initMap(width, height, walls, boxes)
    bombs.foreach(bomb => validatePosition(bomb.x, bomb.y))
    bombs.foreach(bomb => map(bomb.x)(bomb.y) = bomb.countDown)
  }

  // TODO to delete
  def getMap() = map

  def updateMapBox(boxes: Array[Position]): Unit = {
    map.map(_.map(item => if (item == Constant.MAP_BOX) Constant.MAP_BLANK else item))
    boxes.foreach(box => map(box.x)(box.y) = Constant.MAP_BOX)
  }

  def updateMapBombs(bombs: Array[Bomb]): Unit = {
    map.map(_.map(item => if (isBomb(item)) Constant.MAP_BLANK else item))
    bombs.foreach(addOneBomb)
  }

  def addOneBomb(bomb: Bomb): Unit = {
    validatePosition(bomb.x, bomb.y)
    map(bomb.x)(bomb.y) = bomb.countDown
  }

  def placeNewBomb(x: Int, y: Int) = {
    validatePosition(x, y)
    map(x)(y) = Constant.MAP_BOMB_COUNT_3
  }

  private def validatePosition(x: Int, y: Int): Unit = {
    if (isOutOfMapBounds(x, y)) {
      throw new InvalidPositionException(x, y)
    }
  }

  def isBombInPosition(pos: Position): Boolean = isBombInPosition(pos.x, pos.y)

  def isBombInPosition(x: Int, y: Int): Boolean = {
    validatePosition(x, y)
    isBomb(map(x)(y))
  }

  /**
   * print map info
   * 左上角为地图（0，0）右下角为（width， height）
   * 第一行为  （x， 0）     x为0-width
   * 最后一行为 （x， height） x为0-width
   *
   * 0     表示该位置为空
   * $     表示该位置为箱子
   * #     表示该位置为墙
   * ?     其他未知情况
   *
   * @return
   */
  def printMap(): Unit = printMap(map)

  def printMap(map: Array[Array[Int]]): Unit = {
    if (map == null) {
      println("The map has NOT been initialized yet.")
    } else {
      val mapChars = map.map(_.map(item =>
        item match {
          case Constant.MAP_BLANK => '*'
          case v: Int if isBomb(v) => '@'
          case Constant.MAP_BOX => '$'
          case Constant.MAP_WALL => '#'
          case _ => '?'
        }))
      for (i <- 0 until height) {
        println(mapChars.map(_(i)).mkString(" "))
      }
    }
  }

  def getDangerPositions(): Array[Position] = {
    val bombs: Array[Bomb] = map.flatMap(row => {
      val x = map.indexOf(row)
      (0 until height).zip(row).filter(it => isBomb(it._2))
        .map(it => Bomb(0, x, it._1, it._2))
    })
    getDangerPositions(bombs)
  }

  private def isBomb(item: Int) = item > Constant.MAP_BLANK && item <= Constant.MAP_BOMB_COUNT_3

  /**
   * return all the positions that are in the range of bombs' explotion
   *
   * @param bombs
   * @return
   */
  def getDangerPositions(bombs: Array[Bomb]): Array[Position] = getDangerPositions(bombs, Constant.BOMB_RANGE)

  /**
   * return all the positions that are in the range of bombs' explotion
   *
   * @param bombs
   * @param bombRange
   * @return
   */
  def getDangerPositions(bombs: Array[Bomb], bombRange: Int): Array[Position] = {
    val copyMap = map.clone()
    val dangerBombs = bombs.filter(bomb => isDangeriousBomb(bomb.countDown))

    def markOneDirection(pos: (Int, Int), d: (Int, Int)): Unit = {
      for (i <- 1 to bombRange) {
        val next = (pos._1 + d._1 * i, pos._2 + d._2 * i)
        // 如果此方向已经超出边界，或者是墙（炸弹无法穿透无法炸毁），则不标记也无需继续
        if (isOutOfMapBounds(next) || isWall(copyMap(next._1)(next._2))) {
          return
        }
        // 如果此方向是箱子和炸弹，可以炸毁，但炸弹威力无法穿透，标记后无需继续
        if (isBreakable(copyMap(next._1)(next._2))) {
          copyMap(next._1)(next._2) = Constant.MAP_BOMB_COUNT_1
          return
        } else {
          copyMap(next._1)(next._2) = Constant.MAP_BOMB_COUNT_1
        }
      }
    }

    def markOneBomb: (Bomb) => Unit = {
      bomb => {
        val directions = List((-1, 0), (1, 0), (0, -1), (0, 1))
        directions.foreach(d => markOneDirection((bomb.x, bomb.y), d))
      }
    }

    dangerBombs.foreach(markOneBomb)

    var otherBombs = bombs.filterNot(bomb => isDangeriousBomb(bomb.countDown))
    var explodeAreas: Array[(Int, Int)] = Utils.getValuePositionsInDim2Array(copyMap, Constant.MAP_BOMB_COUNT_1)
    var connectedBombs = otherBombs.filter(bomb => explodeAreas.contains((bomb.x, bomb.y)))
    while (connectedBombs.nonEmpty) {
      connectedBombs.foreach(markOneBomb)
      // update explodeAreas, otherBombs, connectedBombs
      explodeAreas = Utils.getValuePositionsInDim2Array(copyMap, Constant.MAP_BOMB_COUNT_1)
      connectedBombs = otherBombs.filter(bomb => explodeAreas.contains((bomb.x, bomb.y)))
      otherBombs = otherBombs.filterNot(bomb => explodeAreas.contains((bomb.x, bomb.y)))
    }

    explodeAreas.map(pos => Position(pos._1, pos._2))
  }

  private def isWall(item: Int): Boolean = item == Constant.MAP_WALL

  private def isBreakable(item: Int): Boolean = isBomb(item) || isBox(item)

  private def isBox(item: Int): Boolean = item == Constant.MAP_BOX

  private def isOutOfMapBounds(xy: (Int, Int)): Boolean = isOutOfMapBounds(xy._1, xy._2)

  private def isOutOfMapBounds(x: Int, y: Int): Boolean = x < 0 || y < 0 || x >= width || y >= height

  private def isDangeriousBomb(item: Int) = item == Constant.MAP_BOMB_COUNT_1

  def nextDirections(pos: Position): List[DirectionEnum] =  nextPossible(pos).map(_._1)

  def nextPositons(pos: Position): List[Position] = nextPossible(pos).map(_._2)

  /**
   * 给定player位置，获取所有可能走的路径
   *
   * @param pos
   * @param isBoost
   * @return
   */
  def possiblePath(pos: Position, isBoost: Boolean) = {

    val moveStill = BomberPath(pos)

    val possible: List[(DirectionEnum, Position)] = nextPossible(pos)

    val oneStepPath = possible.map(it => BomberPath(pos, it._1))
    if(isBoost) {
      val twoStepPath = possible.flatMap(it => {
        val d1 = it._1
        nextDirections(it._2).filter(!d1.isOppsiteTo(_)).map(d2 => BomberPath(pos, d1, d2))
      })
      moveStill :: oneStepPath ::: twoStepPath
    } else moveStill :: oneStepPath
  }

  private def nextPossible(pos: Position) = {
    val initDirections = List(DirectionEnum.UP, DirectionEnum.DOWN, DirectionEnum.LEFT, DirectionEnum.RIGHT)

    initDirections.map(d => (d, Position(pos.x + d.getDx, pos.y + d.getDy)))
      .filter(pair => !isOutOfMapBounds(pair._2.x, pair._2.y))
      .filter(pair => isBlank(map(pair._2.x)(pair._2.y)))
  }

  def nextSafeDirections(pos: Position): List[DirectionEnum] = {

    val initDirections = List(DirectionEnum.UP, DirectionEnum.DOWN, DirectionEnum.LEFT, DirectionEnum.RIGHT)

    initDirections.map(d => (d, Position(pos.x + d.getDx, pos.y + d.getDy)))
      .filter(pair => !isOutOfMapBounds(pair._2.x, pair._2.y))
      .filter(pair => isBlank(map(pair._2.x)(pair._2.y)))
      .filter(pair => !getDangerPositions().contains(pair._2))
      .map(_._1)
  }

  private def isBlank(item: Int): Boolean = item == Constant.MAP_BLANK

  private def validatePosition(pos: Position): Unit = validatePosition(pos.x, pos.y)

}

class InvalidPositionException(val x: Int, val y: Int) extends Exception {

  override def toString = s"InvalidPositionException($x, $y)"
}
