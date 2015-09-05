package com.huawei.bomber

import java.util

import akka.actor.{Actor, ActorSystem, Props}
import akka.routing.RoundRobinRouter
import com.huawei.bomber.model.{Bomb, BomberPath, Position}

import scala.annotation.tailrec
import scala.collection.JavaConversions._

/**
 * Created by frank on 9/1/15.
 */
object SearchActorTest extends App {

  // init map
  val walls: Array[Position] = Array(Position(1, 1), Position(4, 3))
  val boxes: Array[Position] = Array(Position(2, 2), Position(3, 3))
  val bombs: Array[Bomb] = Array(Bomb(1, 3, 1, 1), Bomb(2, 2, 4, 1), Bomb(1, 0, 4, 2), Bomb(2, 2, 1, 3))
  MapAnalyzer.initMap(8, 8, walls, boxes, bombs)
  MapAnalyzer.printMap()


  val system = ActorSystem("bomber-system")
  // default Actor constructor
  //  val listener = system.actorOf(Props[Listener], name = "listener")

  val positions = Array(Position(0, 0), Position(1, 3))
  val isBoosts = Array(false, true)

  val brainActor = system.actorOf(Props(new CommandActor(positions, isBoosts)), name = "command-actor")

  brainActor ! StartSearch
}

sealed trait BomberMessage

case class SearchNextStep(path: BomberPath) extends BomberMessage

case class SearchPath(start: Position, isBoost: Boolean) extends BomberMessage

case class PathResult(paths: List[BomberPath]) extends BomberMessage

case class NoSafePathResult(paths: List[BomberPath]) extends BomberMessage

case object StartSearch extends BomberMessage

case class Action(str: String) extends BomberMessage

case object NoPath extends BomberMessage

case object PlanAction extends BomberMessage

case class EvaluateSolution(solution: Array[BomberPath]) extends BomberMessage

class PlayerActor extends Actor {

  def receive = {
    case SearchPath(start, isBoost) => {
      val possiblePath = MapAnalyzer.possiblePath(start, isBoost)
      val dangerPosArr = MapAnalyzer.getDangerPositions()
      val safePath = possiblePath.filter(path => !dangerPosArr.contains(path))
      if (safePath.isEmpty) {
        // 如果没有找到安全路线，则发送NoSafePathResult消息，将可能路径发送
        sender ! NoSafePathResult(possiblePath)
      } else {
        sender ! PathResult(safePath)
      }
    }
  }
}

class PlanActor(val allSolutions: Array[Array[BomberPath]]) extends Actor {

  val solutionNum = allSolutions.length

  val router = context.actorOf(Props[EvaluateActor].withRouter(RoundRobinRouter(solutionNum)), name = "plan-router")

  def receive = {
    case PlanAction => allSolutions.foreach(solution => router ! EvaluateSolution(solution))

    // TODO receive result

  }
}

class EvaluateActor extends Actor {

  def receive = {
    case EvaluateSolution(solution) => {
      // 所有可能放置炸弹的点
      val bombPoss = solution.map(path => {
        val initBombList = List(path.start)
        if (path.countSteps() > 0) {
          val destination = path.getDestination()
          destination :: initBombList
        } else initBombList
      }).toList
      println(bombPoss.mkString(";"))

      val playerPoss = solution.map(_.getDestination())


    }
  }

}


class CommandActor(val playerPositions: Array[Position], val isBoosts: Array[Boolean]) extends Actor {

  val playerNum = playerPositions.length

  val router = context.actorOf(Props[PlayerActor].withRouter(RoundRobinRouter(playerNum)), name = "player-router")
  val results = new util.ArrayList[List[BomberPath]]()
  var resultNum = 0

  def receive = {

    case StartSearch => for (i <- 0 until playerNum) router ! SearchPath(playerPositions(i), isBoosts(i))

    case PathResult(paths) => {
      println("got a result")
      paths.foreach(p => println(p.getFootprint()))
      results.add(paths)
      synchronized {
        resultNum += 1
        checkAndSendMsg
      }
    }

    case NoSafePathResult(paths) => {
      // TODO
      println("got a danger result")
      results add paths
      synchronized {
        resultNum += 1
        checkAndSendMsg
      }
    }

  }


  def checkAndSendMsg: Unit = {
    if (resultNum == playerNum) {
      val solutions = generatorAllSolutions()
      println(s"solution count -> ${solutions.length}")
      solutions.foreach(s => {
        println("solution: ")
        s.foreach(println)
      })
      val planActor = context.actorOf(Props(new PlanActor(solutions)), name = "plan-actor")
      planActor ! PlanAction
    }
  }

  @tailrec
  private def rec(list: List[List[BomberPath]], restPlayerPaths: List[List[BomberPath]]): List[List[BomberPath]] = {
    restPlayerPaths match {
      case Nil => list

      case head :: tail => {
        val newList = head.flatMap(p => {
          list.map(pathList => p :: pathList)
        })
        rec(newList, tail)
      }
    }
  }

  private def generatorAllSolutions(): Array[Array[BomberPath]] = {

    val head :: tail = results.toList

    val initList = head.map(p => List(p))
    val solutions = rec(initList, tail)
    solutions.map(_.toArray).toArray
  }
}



