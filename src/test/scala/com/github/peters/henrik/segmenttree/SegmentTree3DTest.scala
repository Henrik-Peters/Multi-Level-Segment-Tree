// ---------------------------------------------------------------------
// MIT License
// Copyright (c) 2018 Henrik Peters
// See LICENSE file in the project root for full license information.
// ---------------------------------------------------------------------
package com.github.peters.henrik.segmenttree

import Range._
import scala.util.Random
import org.scalatest.{FlatSpec, Matchers}

class SegmentTree3DTest extends FlatSpec with Matchers {

  object IntegerAddition extends Monoid[Int] {
    def fold(a: Int, b: Int): Int = a + b
    def identity: Int = 0
  }

  val tensor = Seq(
    Seq(Seq(1, 2), Seq(3, 4)),
    Seq(Seq(5, 6), Seq(7, 8))
  )

  val tree: SegmentTree3D[Int] = SegmentTree3D.fromTensor(tensor, IntegerAddition)

  "query for a 3-dimensional tree with range" should "be the same as with ints" in {
    for (z <- 0 to 1) {
      for (y <- 0 to 1) {
        for (x <- 0 to 1) {
          val zRange = Range(z, z)
          val yRange = Range(y, y)
          val xRange = Range(x, x)
          tree.query(zRange)(yRange)(xRange).get shouldEqual tree.query(z, z)(y, y)(x, x).get
        }
      }
    }
  }

  "query for the 3-dimensional tree with ints" should "be the folded value" in {
    tree.query(0, 1)(0, 1)(0, 1).get shouldEqual 36
  }

  "query for the 3-dimensional tree for a single leaf value" should "be the leaf value" in {
    tree.query(Range(0, 0))(Range(0, 0))(Range(0, 0)).get shouldEqual 1
    tree.query(Range(0, 0))(Range(0, 0))(Range(1, 1)).get shouldEqual 2
    tree.query(Range(0, 0))(Range(1, 1))(Range(0, 0)).get shouldEqual 3
    tree.query(Range(0, 0))(Range(1, 1))(Range(1, 1)).get shouldEqual 4
    tree.query(Range(1, 1))(Range(0, 0))(Range(0, 0)).get shouldEqual 5
    tree.query(Range(1, 1))(Range(0, 0))(Range(1, 1)).get shouldEqual 6
    tree.query(Range(1, 1))(Range(1, 1))(Range(0, 0)).get shouldEqual 7
    tree.query(Range(1, 1))(Range(1, 1))(Range(1, 1)).get shouldEqual 8
  }

  "query for the 3-dimensional tree with a complete z-stack" should "be the folded value" in {
    tree.query(Range(0, 1))(Range(0, 0))(Range(0, 0)).get shouldEqual 6
    tree.query(Range(0, 1))(Range(0, 0))(Range(1, 1)).get shouldEqual 8
    tree.query(Range(0, 1))(Range(1, 1))(Range(0, 0)).get shouldEqual 10
    tree.query(Range(0, 1))(Range(1, 1))(Range(1, 1)).get shouldEqual 12
  }

  "query for the 3-dimensional tree with a complete y-stack" should "be the folded value" in {
    tree.query(Range(0, 0))(Range(0, 1))(Range(0, 0)).get shouldEqual 4
    tree.query(Range(0, 0))(Range(0, 1))(Range(1, 1)).get shouldEqual 6
    tree.query(Range(1, 1))(Range(0, 1))(Range(0, 0)).get shouldEqual 12
    tree.query(Range(1, 1))(Range(0, 1))(Range(1, 1)).get shouldEqual 14
  }

  "query for the 3-dimensional tree with a complete x-stack" should "be the folded value" in {
    tree.query(Range(0, 0))(Range(0, 0))(Range(0, 1)).get shouldEqual 3
    tree.query(Range(0, 0))(Range(1, 1))(Range(0, 1)).get shouldEqual 7
    tree.query(Range(1, 1))(Range(0, 0))(Range(0, 1)).get shouldEqual 11
    tree.query(Range(1, 1))(Range(1, 1))(Range(0, 1)).get shouldEqual 15
  }

  "query for the 3-dimensional tree with a complete z-face" should "be the folded value" in {
    tree.query(Range(0, 1))(Range(0, 0))(Range(0, 1)).get shouldEqual 14
    tree.query(Range(0, 1))(Range(1, 1))(Range(0, 1)).get shouldEqual 22
  }

  "query for the 3-dimensional tree with a complete y-face" should "be the folded value" in {
    tree.query(Range(0, 0))(Range(0, 1))(Range(0, 1)).get shouldEqual 10
    tree.query(Range(1, 1))(Range(0, 1))(Range(0, 1)).get shouldEqual 26
  }

  "query for the 3-dimensional tree with a complete x-face" should "be the folded value" in {
    tree.query(Range(0, 1))(Range(0, 1))(Range(0, 0)).get shouldEqual 16
    tree.query(Range(0, 1))(Range(0, 1))(Range(1, 1)).get shouldEqual 20
  }

  "modify for the element 1" should "update the leaf value" in {
    assert(tree.modify(0)(0)(0)(100))
    tree.query(Range(0, 0))(Range(0, 0))(Range(0, 0)).get shouldEqual 100
    tree.query(Range(0, 0))(Range(0, 0))(Range(1, 1)).get shouldEqual 2
    tree.query(Range(0, 0))(Range(1, 1))(Range(0, 0)).get shouldEqual 3
    tree.query(Range(0, 0))(Range(1, 1))(Range(1, 1)).get shouldEqual 4
    tree.query(Range(1, 1))(Range(0, 0))(Range(0, 0)).get shouldEqual 5
    tree.query(Range(1, 1))(Range(0, 0))(Range(1, 1)).get shouldEqual 6
    tree.query(Range(1, 1))(Range(1, 1))(Range(0, 0)).get shouldEqual 7
    tree.query(Range(1, 1))(Range(1, 1))(Range(1, 1)).get shouldEqual 8
  }

  "modify for the element 2" should "update the leaf value" in {
    assert(tree.modify(0)(0)(1)(200))
    tree.query(Range(0, 0))(Range(0, 0))(Range(0, 0)).get shouldEqual 100
    tree.query(Range(0, 0))(Range(0, 0))(Range(1, 1)).get shouldEqual 200
    tree.query(Range(0, 0))(Range(1, 1))(Range(0, 0)).get shouldEqual 3
    tree.query(Range(0, 0))(Range(1, 1))(Range(1, 1)).get shouldEqual 4
    tree.query(Range(1, 1))(Range(0, 0))(Range(0, 0)).get shouldEqual 5
    tree.query(Range(1, 1))(Range(0, 0))(Range(1, 1)).get shouldEqual 6
    tree.query(Range(1, 1))(Range(1, 1))(Range(0, 0)).get shouldEqual 7
    tree.query(Range(1, 1))(Range(1, 1))(Range(1, 1)).get shouldEqual 8
  }

  "modify for the element 3" should "update the leaf value" in {
    assert(tree.modify(0)(1)(0)(300))
    tree.query(Range(0, 0))(Range(0, 0))(Range(0, 0)).get shouldEqual 100
    tree.query(Range(0, 0))(Range(0, 0))(Range(1, 1)).get shouldEqual 200
    tree.query(Range(0, 0))(Range(1, 1))(Range(0, 0)).get shouldEqual 300
    tree.query(Range(0, 0))(Range(1, 1))(Range(1, 1)).get shouldEqual 4
    tree.query(Range(1, 1))(Range(0, 0))(Range(0, 0)).get shouldEqual 5
    tree.query(Range(1, 1))(Range(0, 0))(Range(1, 1)).get shouldEqual 6
    tree.query(Range(1, 1))(Range(1, 1))(Range(0, 0)).get shouldEqual 7
    tree.query(Range(1, 1))(Range(1, 1))(Range(1, 1)).get shouldEqual 8
  }

  "modify for the element 4" should "update the leaf value" in {
    assert(tree.modify(0)(1)(1)(400))
    tree.query(Range(0, 0))(Range(0, 0))(Range(0, 0)).get shouldEqual 100
    tree.query(Range(0, 0))(Range(0, 0))(Range(1, 1)).get shouldEqual 200
    tree.query(Range(0, 0))(Range(1, 1))(Range(0, 0)).get shouldEqual 300
    tree.query(Range(0, 0))(Range(1, 1))(Range(1, 1)).get shouldEqual 400
    tree.query(Range(1, 1))(Range(0, 0))(Range(0, 0)).get shouldEqual 5
    tree.query(Range(1, 1))(Range(0, 0))(Range(1, 1)).get shouldEqual 6
    tree.query(Range(1, 1))(Range(1, 1))(Range(0, 0)).get shouldEqual 7
    tree.query(Range(1, 1))(Range(1, 1))(Range(1, 1)).get shouldEqual 8
  }

  "modify for the element 5" should "update the leaf value" in {
    assert(tree.modify(1)(0)(0)(500))
    tree.query(Range(0, 0))(Range(0, 0))(Range(0, 0)).get shouldEqual 100
    tree.query(Range(0, 0))(Range(0, 0))(Range(1, 1)).get shouldEqual 200
    tree.query(Range(0, 0))(Range(1, 1))(Range(0, 0)).get shouldEqual 300
    tree.query(Range(0, 0))(Range(1, 1))(Range(1, 1)).get shouldEqual 400
    tree.query(Range(1, 1))(Range(0, 0))(Range(0, 0)).get shouldEqual 500
    tree.query(Range(1, 1))(Range(0, 0))(Range(1, 1)).get shouldEqual 6
    tree.query(Range(1, 1))(Range(1, 1))(Range(0, 0)).get shouldEqual 7
    tree.query(Range(1, 1))(Range(1, 1))(Range(1, 1)).get shouldEqual 8
  }

  "modify for the element 6" should "update the leaf value" in {
    assert(tree.modify(1)(0)(1)(600))
    tree.query(Range(0, 0))(Range(0, 0))(Range(0, 0)).get shouldEqual 100
    tree.query(Range(0, 0))(Range(0, 0))(Range(1, 1)).get shouldEqual 200
    tree.query(Range(0, 0))(Range(1, 1))(Range(0, 0)).get shouldEqual 300
    tree.query(Range(0, 0))(Range(1, 1))(Range(1, 1)).get shouldEqual 400
    tree.query(Range(1, 1))(Range(0, 0))(Range(0, 0)).get shouldEqual 500
    tree.query(Range(1, 1))(Range(0, 0))(Range(1, 1)).get shouldEqual 600
    tree.query(Range(1, 1))(Range(1, 1))(Range(0, 0)).get shouldEqual 7
    tree.query(Range(1, 1))(Range(1, 1))(Range(1, 1)).get shouldEqual 8
  }

  "modify for the element 7" should "update the leaf value" in {
    assert(tree.modify(1)(1)(0)(700))
    tree.query(Range(0, 0))(Range(0, 0))(Range(0, 0)).get shouldEqual 100
    tree.query(Range(0, 0))(Range(0, 0))(Range(1, 1)).get shouldEqual 200
    tree.query(Range(0, 0))(Range(1, 1))(Range(0, 0)).get shouldEqual 300
    tree.query(Range(0, 0))(Range(1, 1))(Range(1, 1)).get shouldEqual 400
    tree.query(Range(1, 1))(Range(0, 0))(Range(0, 0)).get shouldEqual 500
    tree.query(Range(1, 1))(Range(0, 0))(Range(1, 1)).get shouldEqual 600
    tree.query(Range(1, 1))(Range(1, 1))(Range(0, 0)).get shouldEqual 700
    tree.query(Range(1, 1))(Range(1, 1))(Range(1, 1)).get shouldEqual 8
  }

  "modify for the element 8" should "update the leaf value" in {
    assert(tree.modify(1)(1)(1)(800))
    tree.query(Range(0, 0))(Range(0, 0))(Range(0, 0)).get shouldEqual 100
    tree.query(Range(0, 0))(Range(0, 0))(Range(1, 1)).get shouldEqual 200
    tree.query(Range(0, 0))(Range(1, 1))(Range(0, 0)).get shouldEqual 300
    tree.query(Range(0, 0))(Range(1, 1))(Range(1, 1)).get shouldEqual 400
    tree.query(Range(1, 1))(Range(0, 0))(Range(0, 0)).get shouldEqual 500
    tree.query(Range(1, 1))(Range(0, 0))(Range(1, 1)).get shouldEqual 600
    tree.query(Range(1, 1))(Range(1, 1))(Range(0, 0)).get shouldEqual 700
    tree.query(Range(1, 1))(Range(1, 1))(Range(1, 1)).get shouldEqual 800
  }

  class LoopQuery3D(matrix: Seq[Seq[Seq[Int]]], monoid: Monoid[Int]){
    val array: Array[Array[Array[Int]]] = matrix.toArray.map(_.toArray.map(_.toArray))

    def query(zRange: Range)(yRange: Range)(xRange: Range): Option[Int] = {
      try {
        var accumulator = monoid.identity

        for (z <- zRange.start to zRange.end) {
          for (y <- yRange.start to yRange.end) {
            for (x <- xRange.start to xRange.end) {
              accumulator = monoid.fold(accumulator, array(z)(y)(x))
            }
          }
        }

        Some(accumulator)
      } catch {
        case _: Throwable => None
      }
    }

    def modify(z: Int)(y: Int)(x: Int)(newValue: Int): Boolean = {
      try {
        array(z)(y)(x) = newValue
        true
      } catch {
        case _: Throwable => false
      }
    }
  }

  val generate3DSubranges: Seq[Seq[Seq[Int]]] => List[(Range,Range,Range)] = tensor => {
    val zRootRange = Range(0, tensor.length - 1)
    val yRootRange = Range(0, tensor.head.length - 1)
    val xRootRange = Range(0, tensor.head.head.length - 1)

    for {
      z <- subranges(zRootRange)
      y <- subranges(yRootRange)
      x <- subranges(xRootRange)
    } yield (z, y, x)
  }

  val generate3DInvalidSubranges: Seq[Seq[Seq[Int]]] => List[(Range,Range,Range)] = tensor => {
    val zRootRange = Range(0, tensor.length - 1)
    val yRootRange = Range(0, tensor.head.length - 1)
    val xRootRange = Range(0, tensor.head.head.length - 1)

    val zRightRange = Range(zRootRange.end + 1, zRootRange.end + 2)
    val yRightRange = Range(yRootRange.end + 1, yRootRange.end + 2)
    val xRightRange = Range(xRootRange.end + 1, xRootRange.end + 2)
    val negativeRange = Range(-2, -1)

    val rightOf = for {
      z <- subranges(zRightRange)
      y <- subranges(yRightRange)
      x <- subranges(xRightRange)
    } yield (z, y, x)

    val leftOf = for {
      z <- subranges(negativeRange)
      y <- subranges(negativeRange)
      x <- subranges(negativeRange)
    } yield (z, y, x)

    rightOf ::: leftOf
  }

  val test: Seq[Seq[Seq[Int]]] => Unit = tensor => {
    val loopQuery = new LoopQuery3D(tensor, IntegerAddition)
    val tree: SegmentTree3D[Int] = SegmentTree3D.fromTensor(tensor, IntegerAddition)

    val valids = generate3DSubranges(tensor)
    val invalids = generate3DInvalidSubranges(tensor)

    def testQueries(): Unit = {
      valids.foreach(triple => {
        val treeResult = tree.query(triple._1)(triple._2)(triple._3)
        val loopResult = loopQuery.query(triple._1)(triple._2)(triple._3)

        treeResult shouldEqual loopResult
        treeResult should not equal Option.empty
        loopResult should not equal Option.empty
      })

      invalids.foreach(triple => {
        val treeResult = tree.query(triple._1)(triple._2)(triple._3)
        val loopResult = loopQuery.query(triple._1)(triple._2)(triple._3)

        treeResult shouldEqual loopResult
        treeResult shouldEqual Option.empty
        loopResult shouldEqual Option.empty
      })
    }

    testQueries()
  }

  "The loop range implementation" should "match with the 3D tree implementation" in {
    test{Seq(
      Seq(Seq(1, 2), Seq(3, 4)),
      Seq(Seq(5, 6), Seq(7, 8))
    )}
  }
}
