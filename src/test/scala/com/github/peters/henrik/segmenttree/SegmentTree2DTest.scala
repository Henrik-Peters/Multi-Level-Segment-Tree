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
}
