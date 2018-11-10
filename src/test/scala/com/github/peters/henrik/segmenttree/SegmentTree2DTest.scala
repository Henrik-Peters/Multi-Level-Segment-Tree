// ---------------------------------------------------------------------
// MIT License
// Copyright (c) 2018 Henrik Peters
// See LICENSE file in the project root for full license information.
// ---------------------------------------------------------------------
package com.github.peters.henrik.segmenttree

import org.scalatest.{FlatSpec, Matchers}

class SegmentTree2DTest extends FlatSpec with Matchers {

  object IntegerAddition extends Monoid[Int] {
    def fold(a: Int, b: Int): Int = a + b
    def identity: Int = 0
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
}
