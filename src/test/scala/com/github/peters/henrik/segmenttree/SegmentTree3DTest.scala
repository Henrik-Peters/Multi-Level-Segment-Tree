// ---------------------------------------------------------------------
// MIT License
// Copyright (c) 2018 Henrik Peters
// See LICENSE file in the project root for full license information.
// ---------------------------------------------------------------------
package com.github.peters.henrik.segmenttree

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
}
