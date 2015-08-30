package com.huawei.bomber.model

import java.util
import scala.collection.JavaConversions._


/**
 * Created by frank on 8/30/15.
 */
class BomberPath {

  private val steps: util.LinkedList[DirectionEnum] = new util.LinkedList[DirectionEnum]()

  def countSteps(): Int = steps.size()

  def addStep(ds: DirectionEnum*) = ds.foreach(steps.add)

  override def toString = {
    val path = steps.map(_.getName).mkString(",")
    s"BomberPath($path)"
  }
}

object BomberPath {

  def apply() = new BomberPath()

  def apply(directions : DirectionEnum*): BomberPath = {

    val path = BomberPath()
    directions.foreach(d => path.steps.add(d))
    path
  }
}
