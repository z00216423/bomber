package com.huawei.bomber.model

import java.util
import scala.collection.JavaConversions._


/**
 * Created by frank on 8/30/15.
 */
class BomberPath(val start: Position) {

  private val steps: util.LinkedList[DirectionEnum] = new util.LinkedList[DirectionEnum]()
  private val footprints: util.LinkedList[Position] = new util.LinkedList[Position]()


  def countSteps(): Int = steps.size()

  def addStep(ds: DirectionEnum*) = {
    ds.foreach(d => {
      steps.add(d)
      footprints.add(getDestination())
    })
  }

  def getDestination(): Position =  steps.foldLeft(start)((pos, d) => Position(pos.x + d.getDx, pos.y + d.getDy))

  override def toString = {
    val path = steps.map(_.getName).mkString(",")
    s"BomberPath($path)"
  }

  def getFootprint() = {
    (start :: footprints.toList).mkString(" -> ")
  }
}

object BomberPath {

  def apply(start: Position) = new BomberPath(start)

  def apply(start: Position, directions : DirectionEnum*): BomberPath = {

    val path = BomberPath(start)
    directions.foreach(d => path.addStep(d))
    path
  }


}
