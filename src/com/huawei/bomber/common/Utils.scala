package com.huawei.bomber.common

import com.huawei.bomber.model.BomberPath

import scala.annotation.tailrec

/**
 * Created by frank on 8/30/15.
 */
object Utils {


  def getValuePositionsInDim2Array[A](dim2Arr: Array[Array[A]], values: A *): Array[(Int, Int)] = {
    filterPositionsInDim2Array(dim2Arr)(values.contains)
  }

  def filterPositionsInDim2Array[A](dim2Arr: Array[Array[A]])(prediction: A => Boolean): Array[(Int, Int)] = {
    dim2Arr.flatMap(row => {
      val x = dim2Arr.indexOf(row)
      (0 until row.length).zip(row)
        .filter(indexedVal => prediction(indexedVal._2))
        .map(indexedVal => (x, indexedVal._1))
    })
  }

  /**
   * 产生所有可行解，从eachXs中每个X选取一个可能的取值，排列组合成所有可行解
   *
   * @param eachXs
   * @tparam A
   * @return
   */
  def generatorAllSolutions[A](eachXs: List[List[A]]): List[List[A]] = {

    val head :: tail = eachXs

    val initSolutionList = head.map(p => List(p))

    rec(initSolutionList, tail)
  }

  @tailrec
  private def rec[A](initSolutions: List[List[A]], restXs: List[List[A]]): List[List[A]] = {

    restXs match {
      case Nil => initSolutions

      case head :: tail => {
        val newSolutions = head.flatMap(p => {
          initSolutions.map(pathList => p :: pathList)
        })
        rec(newSolutions, tail)
      }
    }
  }

}
