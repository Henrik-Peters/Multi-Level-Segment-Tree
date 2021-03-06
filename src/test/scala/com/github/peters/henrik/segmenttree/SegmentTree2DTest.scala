// ---------------------------------------------------------------------
// MIT License
// Copyright (c) 2018 Henrik Peters
// See LICENSE file in the project root for full license information.
// ---------------------------------------------------------------------
package com.github.peters.henrik.segmenttree

import Range._
import scala.util.Random
import org.scalatest.{FlatSpec, Matchers}

class SegmentTree2DTest extends FlatSpec with Matchers {

  object IntegerAddition extends Monoid[Int] {
    def fold(a: Int, b: Int): Int = a + b
    def identity: Int = 0
  }

  object IntegerMultiplication extends Monoid[Int] {
    def fold(a: Int, b: Int): Int = a * b
    def identity: Int = 1
  }

  val simpleMatrix = Seq(
    Seq(1, 2, 3),
    Seq(4, 5, 6),
    Seq(7, 8, 9)
  )

  val tree: SegmentTree2D[Int] = SegmentTree2D.fromMatrix(simpleMatrix, IntegerAddition)

  "query for a two-dimensional tree with range" should "be the same as with ints" in {
    for (y <- 0 to 2) {
      for (x <- 0 to 2) {
        val yRange = Range(y, y)
        val xRange = Range(x, x)
        tree.query(yRange)(xRange).get shouldEqual tree.query(y, y)(x, x).get
      }
    }
  }

  "query for the two-dimensional tree with ints" should "be the folded value" in {
    tree.query(0, 2)(0, 1).get shouldEqual 27
  }

  "query for the two-dimensional tree for a single leaf value" should "be the leaf value" in {
    tree.query(Range(0, 0))(Range(0, 0)).get shouldEqual 1
    tree.query(Range(0, 0))(Range(1, 1)).get shouldEqual 2
    tree.query(Range(0, 0))(Range(2, 2)).get shouldEqual 3
    tree.query(Range(1, 1))(Range(0, 0)).get shouldEqual 4
    tree.query(Range(1, 1))(Range(1, 1)).get shouldEqual 5
    tree.query(Range(1, 1))(Range(2, 2)).get shouldEqual 6
    tree.query(Range(2, 2))(Range(0, 0)).get shouldEqual 7
    tree.query(Range(2, 2))(Range(1, 1)).get shouldEqual 8
    tree.query(Range(2, 2))(Range(2, 2)).get shouldEqual 9
  }

  "query for the two-dimensional tree with a complete row" should "be the folded value" in {
    tree.query(Range(0, 0))(Range(0, 2)).get shouldEqual 6
    tree.query(Range(1, 1))(Range(0, 2)).get shouldEqual 15
    tree.query(Range(2, 2))(Range(0, 2)).get shouldEqual 24
  }

  "query for the two-dimensional tree with a complete column" should "be the folded value" in {
    tree.query(Range(0, 2))(Range(0, 0)).get shouldEqual 12
    tree.query(Range(0, 2))(Range(1, 1)).get shouldEqual 15
    tree.query(Range(0, 2))(Range(2, 2)).get shouldEqual 18
  }

  "query for the two-dimensional tree with 2 elements in a row" should "be the folded value" in {
    tree.query(Range(0, 1))(Range(0, 0)).get shouldEqual 5
    tree.query(Range(1, 2))(Range(0, 0)).get shouldEqual 11
    tree.query(Range(0, 1))(Range(1, 1)).get shouldEqual 7
    tree.query(Range(1, 2))(Range(1, 1)).get shouldEqual 13
    tree.query(Range(0, 1))(Range(2, 2)).get shouldEqual 9
    tree.query(Range(1, 2))(Range(2, 2)).get shouldEqual 15
  }

  "query for the two-dimensional tree with 2 elements in a column" should "be the folded value" in {
    tree.query(Range(0, 0))(Range(0, 1)).get shouldEqual 3
    tree.query(Range(0, 0))(Range(1, 2)).get shouldEqual 5
    tree.query(Range(1, 1))(Range(0, 1)).get shouldEqual 9
    tree.query(Range(1, 1))(Range(1, 2)).get shouldEqual 11
    tree.query(Range(2, 2))(Range(0, 1)).get shouldEqual 15
    tree.query(Range(2, 2))(Range(1, 2)).get shouldEqual 17
  }

  "query for the two-dimensional tree with a 4 element box" should "be the folded value" in {
    tree.query(Range(0, 1))(Range(0, 1)).get shouldEqual 12
    tree.query(Range(0, 1))(Range(1, 2)).get shouldEqual 16
    tree.query(Range(1, 2))(Range(0, 1)).get shouldEqual 24
    tree.query(Range(1, 2))(Range(1, 2)).get shouldEqual 28
  }

  "query for the two-dimensional tree with the complete range" should "be the folded value" in {
    tree.query(Range(0, 2))(Range(0, 2)).get shouldEqual 45
  }

  "modify for the two-dimensional tree with ints" should "update both inner trees" in {
    assert(tree.modify(1)(1){10})
    tree.query(0, 1)(1, 2).get shouldEqual 21
  }

  "modify for the two-dimensional tree with ints" should "not update other values" in {
    tree.query(Range(0, 0))(Range(0, 0)).get shouldEqual 1
    tree.query(Range(0, 0))(Range(1, 1)).get shouldEqual 2
    tree.query(Range(0, 0))(Range(2, 2)).get shouldEqual 3
    tree.query(Range(1, 1))(Range(0, 0)).get shouldEqual 4
    tree.query(Range(1, 1))(Range(1, 1)).get shouldEqual 10
    tree.query(Range(1, 1))(Range(2, 2)).get shouldEqual 6
    tree.query(Range(2, 2))(Range(0, 0)).get shouldEqual 7
    tree.query(Range(2, 2))(Range(1, 1)).get shouldEqual 8
    tree.query(Range(2, 2))(Range(2, 2)).get shouldEqual 9
  }

  val widthMatrix = Seq(
    Seq(5, 7, 3, 2),
    Seq(1, 9, 4, 6),
    Seq(8, 2, 5, 4)
  )

  val wTree: SegmentTree2D[Int] = SegmentTree2D.fromMatrix(widthMatrix, IntegerAddition)

  "query for the width-two-dimensional tree for a single leaf value" should "be the leaf value" in {
    wTree.query(Range(0, 0))(Range(0, 0)).get shouldEqual 5
    wTree.query(Range(0, 0))(Range(1, 1)).get shouldEqual 7
    wTree.query(Range(0, 0))(Range(2, 2)).get shouldEqual 3
    wTree.query(Range(0, 0))(Range(3, 3)).get shouldEqual 2
    wTree.query(Range(1, 1))(Range(0, 0)).get shouldEqual 1
    wTree.query(Range(1, 1))(Range(1, 1)).get shouldEqual 9
    wTree.query(Range(1, 1))(Range(2, 2)).get shouldEqual 4
    wTree.query(Range(1, 1))(Range(3, 3)).get shouldEqual 6
    wTree.query(Range(2, 2))(Range(0, 0)).get shouldEqual 8
    wTree.query(Range(2, 2))(Range(1, 1)).get shouldEqual 2
    wTree.query(Range(2, 2))(Range(2, 2)).get shouldEqual 5
    wTree.query(Range(2, 2))(Range(3, 3)).get shouldEqual 4
  }

  "query for the width-two-dimensional tree with a complete row" should "be the folded value" in {
    wTree.query(Range(0, 0))(Range(0, 3)).get shouldEqual 17
    wTree.query(Range(1, 1))(Range(0, 3)).get shouldEqual 20
    wTree.query(Range(2, 2))(Range(0, 3)).get shouldEqual 19
  }

  "query for the width-two-dimensional tree with a complete column" should "be the folded value" in {
    wTree.query(Range(0, 2))(Range(0, 0)).get shouldEqual 14
    wTree.query(Range(0, 2))(Range(1, 1)).get shouldEqual 18
    wTree.query(Range(0, 2))(Range(2, 2)).get shouldEqual 12
    wTree.query(Range(0, 2))(Range(3, 3)).get shouldEqual 12
  }

  "query for the width-two-dimensional tree with 3 elements in a row" should "be the folded value" in {
    wTree.query(Range(0, 0))(Range(0, 2)).get shouldEqual 15
    wTree.query(Range(0, 0))(Range(1, 3)).get shouldEqual 12
    wTree.query(Range(1, 1))(Range(0, 2)).get shouldEqual 14
    wTree.query(Range(1, 1))(Range(1, 3)).get shouldEqual 19
    wTree.query(Range(2, 2))(Range(0, 2)).get shouldEqual 15
    wTree.query(Range(2, 2))(Range(1, 3)).get shouldEqual 11
  }

  "query for the width-two-dimensional tree with 2 elements in a row" should "be the folded value" in {
    wTree.query(Range(0, 0))(Range(0, 1)).get shouldEqual 12
    wTree.query(Range(0, 0))(Range(1, 2)).get shouldEqual 10
    wTree.query(Range(0, 0))(Range(2, 3)).get shouldEqual 5
    wTree.query(Range(1, 1))(Range(0, 1)).get shouldEqual 10
    wTree.query(Range(1, 1))(Range(1, 2)).get shouldEqual 13
    wTree.query(Range(1, 1))(Range(2, 3)).get shouldEqual 10
    wTree.query(Range(2, 2))(Range(0, 1)).get shouldEqual 10
    wTree.query(Range(2, 2))(Range(1, 2)).get shouldEqual 7
    wTree.query(Range(2, 2))(Range(2, 3)).get shouldEqual 9
  }

  "query for the width-two-dimensional tree with 2 elements in a column" should "be the folded value" in {
    wTree.query(Range(0, 1))(Range(0, 0)).get shouldEqual 6
    wTree.query(Range(1, 2))(Range(0, 0)).get shouldEqual 9
    wTree.query(Range(0, 1))(Range(1, 1)).get shouldEqual 16
    wTree.query(Range(1, 2))(Range(1, 1)).get shouldEqual 11
    wTree.query(Range(0, 1))(Range(2, 2)).get shouldEqual 7
    wTree.query(Range(1, 2))(Range(2, 2)).get shouldEqual 9
    wTree.query(Range(0, 1))(Range(3, 3)).get shouldEqual 8
    wTree.query(Range(1, 2))(Range(3, 3)).get shouldEqual 10
  }

  "query for the width-two-dimensional tree with a 4 element box" should "be the folded value" in {
    wTree.query(Range(0, 1))(Range(0, 1)).get shouldEqual 22
    wTree.query(Range(0, 1))(Range(1, 2)).get shouldEqual 23
    wTree.query(Range(0, 1))(Range(2, 3)).get shouldEqual 15
    wTree.query(Range(1, 2))(Range(0, 1)).get shouldEqual 20
    wTree.query(Range(1, 2))(Range(1, 2)).get shouldEqual 20
    wTree.query(Range(1, 2))(Range(2, 3)).get shouldEqual 19
  }

  "query for the width-two-dimensional tree with a 3x2 element box" should "be the folded value" in {
    wTree.query(Range(0, 1))(Range(0, 2)).get shouldEqual 29
    wTree.query(Range(0, 1))(Range(1, 3)).get shouldEqual 31
    wTree.query(Range(1, 2))(Range(0, 2)).get shouldEqual 29
    wTree.query(Range(1, 2))(Range(1, 3)).get shouldEqual 30
  }

  "query for the width-two-dimensional tree with a 2x3 element box" should "be the folded value" in {
    wTree.query(Range(0, 2))(Range(0, 1)).get shouldEqual 32
    wTree.query(Range(0, 2))(Range(1, 2)).get shouldEqual 30
    wTree.query(Range(0, 2))(Range(2, 3)).get shouldEqual 24
  }

  "query for the width-two-dimensional tree with a 3x3 element box" should "be the folded value" in {
    wTree.query(Range(0, 2))(Range(0, 2)).get shouldEqual 44
    wTree.query(Range(0, 2))(Range(1, 3)).get shouldEqual 42
  }

  "query for the width-two-dimensional tree with the complete range" should "be the folded value" in {
    wTree.query(Range(0, 2))(Range(0, 3)).get shouldEqual 56
  }

  "query for the width-two-dimensional tree with an invalid range" should "be empty" in {
    wTree.query(Range(0, 0))(Range(-1, 1)) shouldEqual Option.empty
    wTree.query(Range(1, 1))(Range(-1, 1)) shouldEqual Option.empty
    wTree.query(Range(2, 2))(Range(-1, 1)) shouldEqual Option.empty
    wTree.query(Range(0, 0))(Range(2, 4)) shouldEqual Option.empty
    wTree.query(Range(1, 1))(Range(2, 4)) shouldEqual Option.empty
    wTree.query(Range(2, 2))(Range(2, 4)) shouldEqual Option.empty
    wTree.query(Range(1, 3))(Range(0, 0)) shouldEqual Option.empty
    wTree.query(Range(1, 3))(Range(1, 1)) shouldEqual Option.empty
    wTree.query(Range(1, 3))(Range(2, 2)) shouldEqual Option.empty
    wTree.query(Range(5, 7))(Range(8, 9)) shouldEqual Option.empty
  }

  val heightMatrix = Seq(
    Seq(7, 3, 1),
    Seq(5, 2, 4),
    Seq(1, 8, 6),
    Seq(9, 5, 7)
  )

  val hTree: SegmentTree2D[Int] = SegmentTree2D.fromMatrix(heightMatrix, IntegerAddition)

  "query for the height-two-dimensional tree for a single leaf value" should "be the leaf value" in {
    hTree.query(Range(0, 0))(Range(0, 0)).get shouldEqual 7
    hTree.query(Range(0, 0))(Range(1, 1)).get shouldEqual 3
    hTree.query(Range(0, 0))(Range(2, 2)).get shouldEqual 1
    hTree.query(Range(1, 1))(Range(0, 0)).get shouldEqual 5
    hTree.query(Range(1, 1))(Range(1, 1)).get shouldEqual 2
    hTree.query(Range(1, 1))(Range(2, 2)).get shouldEqual 4
    hTree.query(Range(2, 2))(Range(0, 0)).get shouldEqual 1
    hTree.query(Range(2, 2))(Range(1, 1)).get shouldEqual 8
    hTree.query(Range(2, 2))(Range(2, 2)).get shouldEqual 6
    hTree.query(Range(3, 3))(Range(0, 0)).get shouldEqual 9
    hTree.query(Range(3, 3))(Range(1, 1)).get shouldEqual 5
    hTree.query(Range(3, 3))(Range(2, 2)).get shouldEqual 7
  }

  "query for the height-two-dimensional tree with a complete row" should "be the folded value" in {
    hTree.query(Range(0, 0))(Range(0, 2)).get shouldEqual 11
    hTree.query(Range(1, 1))(Range(0, 2)).get shouldEqual 11
    hTree.query(Range(2, 2))(Range(0, 2)).get shouldEqual 15
    hTree.query(Range(3, 3))(Range(0, 2)).get shouldEqual 21
  }

  "query for the height-two-dimensional tree with a complete column" should "be the folded value" in {
    hTree.query(Range(0, 3))(Range(0, 0)).get shouldEqual 22
    hTree.query(Range(0, 3))(Range(1, 1)).get shouldEqual 18
    hTree.query(Range(0, 3))(Range(2, 2)).get shouldEqual 18
  }

  "query for the height-two-dimensional tree with 3 elements in a column" should "be the folded value" in {
    hTree.query(Range(0, 2))(Range(0, 0)).get shouldEqual 13
    hTree.query(Range(1, 3))(Range(0, 0)).get shouldEqual 15
    hTree.query(Range(0, 2))(Range(1, 1)).get shouldEqual 13
    hTree.query(Range(1, 3))(Range(1, 1)).get shouldEqual 15
    hTree.query(Range(0, 2))(Range(2, 2)).get shouldEqual 11
    hTree.query(Range(1, 3))(Range(2, 2)).get shouldEqual 17
  }

  "query for the height-two-dimensional tree with 2 elements in a column" should "be the folded value" in {
    hTree.query(Range(0, 1))(Range(0, 0)).get shouldEqual 12
    hTree.query(Range(1, 2))(Range(0, 0)).get shouldEqual 6
    hTree.query(Range(2, 3))(Range(0, 0)).get shouldEqual 10
    hTree.query(Range(0, 1))(Range(1, 1)).get shouldEqual 5
    hTree.query(Range(1, 2))(Range(1, 1)).get shouldEqual 10
    hTree.query(Range(2, 3))(Range(1, 1)).get shouldEqual 13
    hTree.query(Range(0, 1))(Range(2, 2)).get shouldEqual 5
    hTree.query(Range(1, 2))(Range(2, 2)).get shouldEqual 10
    hTree.query(Range(2, 3))(Range(2, 2)).get shouldEqual 13
  }

  "query for the height-two-dimensional tree with 2 elements in a row" should "be the folded value" in {
    hTree.query(Range(0, 0))(Range(0, 1)).get shouldEqual 10
    hTree.query(Range(0, 0))(Range(1, 2)).get shouldEqual 4
    hTree.query(Range(1, 1))(Range(0, 1)).get shouldEqual 7
    hTree.query(Range(1, 1))(Range(1, 2)).get shouldEqual 6
    hTree.query(Range(2, 2))(Range(0, 1)).get shouldEqual 9
    hTree.query(Range(2, 2))(Range(1, 2)).get shouldEqual 14
    hTree.query(Range(3, 3))(Range(0, 1)).get shouldEqual 14
    hTree.query(Range(3, 3))(Range(1, 2)).get shouldEqual 12
  }

  "query for the height-two-dimensional tree with a 4 element box" should "be the folded value" in {
    hTree.query(Range(0, 1))(Range(0, 1)).get shouldEqual 17
    hTree.query(Range(0, 1))(Range(1, 2)).get shouldEqual 10
    hTree.query(Range(1, 2))(Range(0, 1)).get shouldEqual 16
    hTree.query(Range(1, 2))(Range(1, 2)).get shouldEqual 20
    hTree.query(Range(2, 3))(Range(0, 1)).get shouldEqual 23
    hTree.query(Range(2, 3))(Range(1, 2)).get shouldEqual 26
  }

  "query for the height-two-dimensional tree with a 3x2 element box" should "be the folded value" in {
    hTree.query(Range(0, 1))(Range(0, 2)).get shouldEqual 22
    hTree.query(Range(1, 2))(Range(0, 2)).get shouldEqual 26
    hTree.query(Range(2, 3))(Range(0, 2)).get shouldEqual 36
  }

  "query for the height-two-dimensional tree with a 2x3 element box" should "be the folded value" in {
    hTree.query(Range(0, 2))(Range(0, 1)).get shouldEqual 26
    hTree.query(Range(0, 2))(Range(1, 2)).get shouldEqual 24
    hTree.query(Range(1, 3))(Range(0, 1)).get shouldEqual 30
    hTree.query(Range(1, 3))(Range(1, 2)).get shouldEqual 32
  }

  "query for the height-two-dimensional tree with a 3x3 element box" should "be the folded value" in {
    hTree.query(Range(0, 2))(Range(0, 2)).get shouldEqual 37
    hTree.query(Range(1, 3))(Range(0, 2)).get shouldEqual 47
  }

  "query for the height-two-dimensional tree with the complete range" should "be the folded value" in {
    hTree.query(Range(0, 3))(Range(0, 2)).get shouldEqual 58
  }

  "query for the height-two-dimensional tree with an invalid range" should "be empty" in {
    hTree.query(Range(0, 0))(Range(-1, 1)) shouldEqual Option.empty
    hTree.query(Range(1, 1))(Range(-1, 1)) shouldEqual Option.empty
    hTree.query(Range(2, 2))(Range(-1, 1)) shouldEqual Option.empty
    hTree.query(Range(3, 3))(Range(-1, 1)) shouldEqual Option.empty
    hTree.query(Range(0, 0))(Range(2, 3)) shouldEqual Option.empty
    hTree.query(Range(1, 1))(Range(2, 3)) shouldEqual Option.empty
    hTree.query(Range(2, 2))(Range(2, 3)) shouldEqual Option.empty
    hTree.query(Range(3, 3))(Range(2, 3)) shouldEqual Option.empty
    hTree.query(Range(-1, 1))(Range(0, 0)) shouldEqual Option.empty
    hTree.query(Range(3, 4))(Range(0, 0)) shouldEqual Option.empty
    hTree.query(Range(7, 8))(Range(5, 7)) shouldEqual Option.empty
  }

  "Equals for two identical leafs" should "be true" in {
    val fstTree = SegmentTree2D.fromMatrix(Seq(Seq(1)), IntegerAddition)
    val sndTree = SegmentTree2D.fromMatrix(Seq(Seq(1)), IntegerAddition)
    assert(fstTree.equals(sndTree))
    assert(sndTree.equals(fstTree))
  }

  "Equals for two identical leafs with different monoids" should "be false" in {
    val fstTree = SegmentTree2D.fromMatrix(Seq(Seq(1)), IntegerAddition)
    val sndTree = SegmentTree2D.fromMatrix(Seq(Seq(1)), IntegerMultiplication)
    assert(!fstTree.equals(sndTree))
    assert(!sndTree.equals(fstTree))
  }

  "Equals for two different leafs" should "be false" in {
    val fstTree = SegmentTree2D.fromMatrix(Seq(Seq(1)), IntegerAddition)
    val sndTree = SegmentTree2D.fromMatrix(Seq(Seq(2)), IntegerAddition)
    assert(!fstTree.equals(sndTree))
    assert(!sndTree.equals(fstTree))
  }

  "Equals for two identical trees with 2 elements" should "be true" in {
    val fstTree = SegmentTree2D.fromMatrix(Seq(Seq(1, 2)), IntegerAddition)
    val sndTree = SegmentTree2D.fromMatrix(Seq(Seq(1, 2)), IntegerAddition)
    assert(fstTree.equals(sndTree))
    assert(sndTree.equals(fstTree))
  }

  "Equals for two identical trees with 3 elements" should "be true" in {
    val fstTree = SegmentTree2D.fromMatrix(Seq(Seq(1, 2, 3)), IntegerAddition)
    val sndTree = SegmentTree2D.fromMatrix(Seq(Seq(1, 2, 3)), IntegerAddition)
    assert(fstTree.equals(sndTree))
    assert(sndTree.equals(fstTree))
  }

  "Equals for two different trees with 3 elements" should "be false" in {
    val fstTree = SegmentTree2D.fromMatrix(Seq(Seq(1, 2, 3)), IntegerAddition)
    val sndTree = SegmentTree2D.fromMatrix(Seq(Seq(1, 7, 3)), IntegerAddition)
    assert(!fstTree.equals(sndTree))
    assert(!sndTree.equals(fstTree))
  }

  "Equals for two identical trees with 2 sequences" should "be true" in {
    val fstTree = SegmentTree2D.fromMatrix(Seq(Seq(1), Seq(2)), IntegerAddition)
    val sndTree = SegmentTree2D.fromMatrix(Seq(Seq(1), Seq(2)), IntegerAddition)
    assert(fstTree.equals(sndTree))
    assert(sndTree.equals(fstTree))
  }

  "Equals for two different trees with 2 sequences" should "be false" in {
    val fstTree = SegmentTree2D.fromMatrix(Seq(Seq(1), Seq(2)), IntegerAddition)
    val sndTree = SegmentTree2D.fromMatrix(Seq(Seq(4), Seq(2)), IntegerAddition)
    assert(!fstTree.equals(sndTree))
    assert(!sndTree.equals(fstTree))
  }

  "Equals for two identical trees with 3 sequences" should "be true" in {
    val fstTree = SegmentTree2D.fromMatrix(Seq(Seq(1), Seq(2), Seq(3)), IntegerAddition)
    val sndTree = SegmentTree2D.fromMatrix(Seq(Seq(1), Seq(2), Seq(3)), IntegerAddition)
    assert(fstTree.equals(sndTree))
    assert(sndTree.equals(fstTree))
  }

  "Equals for two different trees with 3 sequences" should "be true" in {
    val fstTree = SegmentTree2D.fromMatrix(Seq(Seq(1), Seq(2), Seq(3)), IntegerAddition)
    val sndTree = SegmentTree2D.fromMatrix(Seq(Seq(1), Seq(2), Seq(5)), IntegerAddition)
    assert(!fstTree.equals(sndTree))
    assert(!sndTree.equals(fstTree))
  }

  "Equals for two identical trees with 3 sequences and multiple elements" should "be true" in {
    val fstTree = SegmentTree2D.fromMatrix(Seq(Seq(1, 2, 3), Seq(4, 5, 6), Seq(7, 8, 9)), IntegerAddition)
    val sndTree = SegmentTree2D.fromMatrix(Seq(Seq(1, 2, 3), Seq(4, 5, 6), Seq(7, 8, 9)), IntegerAddition)
    assert(fstTree.equals(sndTree))
    assert(sndTree.equals(fstTree))
  }

  "Equals for two different trees with a different sequence amount" should "be true" in {
    val fstTree = SegmentTree2D.fromMatrix(Seq(Seq(1, 2), Seq(3, 4), Seq(5, 6)), IntegerAddition)
    val sndTree = SegmentTree2D.fromMatrix(Seq(Seq(1, 2), Seq(3, 4)), IntegerAddition)
    assert(!fstTree.equals(sndTree))
    assert(!sndTree.equals(fstTree))
  }

  class LoopQuery2D(matrix: Seq[Seq[Int]], monoid: Monoid[Int]){
    val array: Array[Array[Int]] = matrix.toArray.map(_.toArray)

    def query(yRange: Range)(xRange: Range): Option[Int] = {
      try {
        var accumulator = monoid.identity

        for (y <- yRange.start to yRange.end) {
          for (x <- xRange.start to xRange.end) {
            accumulator = monoid.fold(accumulator, array(y)(x))
          }
        }

        Some(accumulator)
      } catch {
        case _: Throwable => None
      }
    }

    def modify(y: Int)(x: Int)(newValue: Int): Boolean = {
      try {
        array(y)(x) = newValue
        true
      } catch {
        case _: Throwable => false
      }
    }
  }

  val generate2DSubranges: Seq[Seq[Int]] => List[(Range,Range)] = matrix => {
    val yRootRange = Range(0, matrix.length - 1)
    val xRootRange = Range(0, matrix.head.length - 1)

    for {
      y <- subranges(yRootRange)
      x <- subranges(xRootRange)
    } yield (y, x)
  }

  val generate2DInvalidSubranges: Seq[Seq[Int]] => List[(Range,Range)] = matrix => {
    val yRootRange = Range(0, matrix.length - 1)
    val xRootRange = Range(0, matrix.head.length - 1)

    val yRightRange = Range(matrix.length, matrix.length + 3)
    val xRightRange = Range(matrix.head.length, matrix.head.length + 3)
    val negativeRange = Range(-3, -1)

    val rightOf = for {
      y <- subranges(yRightRange)
      x <- subranges(xRightRange)
    } yield (y, x)

    val leftOf = for {
      y <- subranges(negativeRange)
      x <- subranges(negativeRange)
    } yield (y, x)

    val yInvalids = for {
      y <- subranges(yRightRange)
      x <- subranges(xRootRange)
    } yield (y, x)

    val xInvalids = for {
      y <- subranges(yRootRange)
      x <- subranges(xRightRange)
    } yield (y, x)

    val yTooBig = for {
      y <- List(Range(0, matrix.length))
      x <- subranges(xRootRange)
    } yield (y, x)

    val xTooBig = for {
      y <- subranges(yRootRange)
      x <- List(Range(0, matrix.head.length))
    } yield (y, x)

    leftOf ::: rightOf ::: yInvalids ::: xInvalids ::: yTooBig ::: xTooBig
  }

  val generate2DSingleElementRanges: Seq[Seq[Int]] => List[(Int,Int)] = matrix => {
    val yElements = matrix.indices.toList
    val xElements = matrix.head.indices.toList

    for {
      y <- yElements
      x <- xElements
    } yield (y, x)
  }

  val test: Seq[Seq[Int]] => Unit = matrix => {
    val loopQuery = new LoopQuery2D(matrix, IntegerAddition)
    val tree: SegmentTree2D[Int] = SegmentTree2D.fromMatrix(matrix, IntegerAddition)

    val valids = generate2DSubranges(matrix)
    val invalids = generate2DInvalidSubranges(matrix)
    val elements = generate2DSingleElementRanges(matrix)

    def testQueries(): Unit = {
      valids.foreach(pair => {
        val treeResult = tree.query(pair._1)(pair._2)
        val loopResult = loopQuery.query(pair._1)(pair._2)

        treeResult shouldEqual loopResult
        treeResult should not equal Option.empty
        loopResult should not equal Option.empty
      })

      invalids.foreach(pair => {
        val treeResult = tree.query(pair._1)(pair._2)
        val loopResult = loopQuery.query(pair._1)(pair._2)

        treeResult shouldEqual loopResult
        treeResult shouldEqual Option.empty
        loopResult shouldEqual Option.empty
      })
    }

    testQueries()
    val modifyCycles = 3
    val random = new Random(matrix.head.head)

    for (_ <- 0 until modifyCycles) {
      elements.foreach(elem => {
        val newValue = random.nextInt(300)
        val treeModify = tree.modify(elem._1)(elem._2)(newValue)
        val loopModify = loopQuery.modify(elem._1)(elem._2)(newValue)

        treeModify shouldEqual loopModify
        treeModify shouldEqual true
        loopModify shouldEqual true
        testQueries()
      })
    }
  }

  "The loop range implementation" should "match with the tree implementation" in {
    test{Seq(
      Seq(1, 2, 3),
      Seq(4, 5, 6),
      Seq(7, 8, 9)
    )}

    test{Seq(
      Seq(1, 2),
      Seq(3, 4)
    )}

    test{Seq(
      Seq(5, 1, 9),
      Seq(3, 1, 4),
      Seq(7, 1, 3)
    )}

    test{Seq(
      Seq(9, 3, 1, 7),
      Seq(3, 4, 5, 8)
    )}

    test{Seq(
      Seq(1, 5, 3),
      Seq(8, 1, 7),
      Seq(5, 7, 6),
      Seq(1, 3, 2),
      Seq(8, 5, 3),
      Seq(9, 6, 4)
    )}

    test{Seq(
      Seq(1, 4, 8, 3, 2, 7, 9),
      Seq(9, 3, 1, 4, 1, 8, 3),
      Seq(7, 8, 1, 9, 3, 2, 4)
    )}

    test{Seq(
      Seq(1, 0, 0, 0, 0, 0),
      Seq(0, 1, 0, 0, 0, 0),
      Seq(0, 0, 1, 0, 0, 0),
      Seq(0, 0, 0, 1, 0, 0),
      Seq(0, 0, 0, 0, 1, 0),
      Seq(0, 0, 0, 0, 0, 1)
    )}
  }
}
