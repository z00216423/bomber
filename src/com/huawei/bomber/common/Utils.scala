package com.huawei.bomber.common

/**
 * Created by frank on 8/30/15.
 */
object Utils {

//  def getValuePositionsInDim2Array[A](dim2Arr: Array[Array[A]], value: A): Array[(Int, Int)] = {
//    filterPositionsInDim2Array(dim2Arr)(value.equals)
//  }

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

}
