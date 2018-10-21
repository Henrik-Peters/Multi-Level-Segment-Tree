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

  val tree = new SegmentTree2D(simpleMatrix, IntegerAddition)

  "query for a two-dimensional tree" should "be the folded value" in {
    val q1: SegmentTree[Int] = tree.tree.query(0,1).get //y query range
    val r: Int = q1.query(1,2).get //x query range
    r shouldEqual 16
  }

}
