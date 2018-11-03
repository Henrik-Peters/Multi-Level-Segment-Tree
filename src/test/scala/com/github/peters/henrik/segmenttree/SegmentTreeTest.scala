// ---------------------------------------------------------------------
// MIT License
// Copyright (c) 2018 Henrik Peters
// See LICENSE file in the project root for full license information.
// ---------------------------------------------------------------------
package com.github.peters.henrik.segmenttree

import org.scalatest.{FlatSpec, Matchers}

class SegmentTreeTest extends FlatSpec with Matchers {

  object IntegerAddition extends Monoid[Int] {
    def fold(a: Int, b: Int): Int = a + b
    def identity: Int = 0
  }

  object StringConcatenation extends Monoid[String] {
    def fold(a: String, b: String): String = a concat b
    def identity: String = ""
  }

  "A single element" should "produce a single leaf as tree" in {
    val singleData = Array(5)
    val leafTree = SegmentTree.fromSequence(singleData, IntegerAddition)
    assert(leafTree.query(0, 0).get == 5)
  }

  val data = List(1, 3, 5, 7, 9, 11)
  val tree: SegmentTree[Int] = SegmentTree.fromSequence(data, IntegerAddition)

  "query for the complete interval" should "be the root value" in {
    assert(tree.query(0, 5).get == 36)
    assert(tree.query(Range(0, 5)).get == 36)
  }

  "query for a left exact matching interval" should "be the node value" in {
    assert(tree.query(0, 1).get == 4)
    assert(tree.query(Range(0, 1)).get == 4)
  }

  "query for another left exact matching interval" should "be the node value" in {
    assert(tree.query(0, 2).get == 9)
    assert(tree.query(Range(0, 2)).get == 9)
  }

  "query for a right exact matching interval" should "be the node value" in {
    assert(tree.query(3, 4).get == 16)
    assert(tree.query(Range(3, 4)).get == 16)
  }

  "query for another right exact matching interval" should "be the node value" in {
    assert(tree.query(3, 5).get == 27)
    assert(tree.query(Range(3, 5)).get == 27)
  }

  "query for two left different nodes" should "be the folded value" in {
    assert(tree.query(1, 2).get == 8)
    assert(tree.query(Range(1, 2)).get == 8)
  }

  "query for two right different nodes" should "be the folded value" in {
    assert(tree.query(4, 5).get == 20)
    assert(tree.query(Range(4, 5)).get == 20)
  }

  "query for an interval that splits over the root node" should "be the folded value" in {
    assert(tree.query(2, 3).get == 12)
    assert(tree.query(Range(2, 3)).get == 12)
  }

  "query for an interval that splits more over the root node" should "be the folded value" in {
    assert(tree.query(1, 4).get == 24)
    assert(tree.query(Range(1, 4)).get == 24)
  }

  "query for an interval out of the root range" should "be none" in {
    tree.query(7, 9) should be (None)
    tree.query(Range(7, 9)) should be (None)
  }

  "query for a negative interval out of the root range" should "be none" in {
    tree.query(-5, -2) should be (None)
    tree.query(Range(-5, -2)) should be (None)
  }

  "query for an interval next to the root range bound" should "be none" in {
    tree.query(6, 8) should be (None)
    tree.query(Range(6, 8)) should be (None)
  }

  "query for a negative interval next to the root range bound" should "be none" in {
    tree.query(-5, -1) should be (None)
    tree.query(Range(-5, -1)) should be (None)
  }

  "query for a overlapping interval out of the root range" should "be none" in {
    tree.query(3, 9) should be (None)
    tree.query(Range(3, 9)) should be (None)
  }

  "query for a overlapping negative interval out of the root range" should "be none" in {
    tree.query(-5, 3) should be (None)
    tree.query(Range(-5, 3)) should be (None)
  }

  "modify for the index 0" should "update the leaf and upper folded values" in {
    assert(tree.modify(0, 2))
    assert(tree.query(0, 0).get == 2)
    assert(tree.query(1, 1).get == 3)
    assert(tree.query(2, 2).get == 5)
    assert(tree.query(3, 3).get == 7)
    assert(tree.query(4, 4).get == 9)
    assert(tree.query(5, 5).get == 11)
    assert(tree.query(0, 1).get == 5)
    assert(tree.query(0, 2).get == 10)
    assert(tree.query(0, 5).get == 37)
    assert(tree.query(3, 5).get == 27)
    assert(tree.query(3, 4).get == 16)
  }

  "modify for the index 1" should "update the leaf and upper folded values" in {
    assert(tree.modify(1, 8))
    assert(tree.query(0, 0).get == 2)
    assert(tree.query(1, 1).get == 8)
    assert(tree.query(2, 2).get == 5)
    assert(tree.query(3, 3).get == 7)
    assert(tree.query(4, 4).get == 9)
    assert(tree.query(5, 5).get == 11)
    assert(tree.query(0, 1).get == 10)
    assert(tree.query(0, 2).get == 15)
    assert(tree.query(0, 5).get == 42)
    assert(tree.query(3, 5).get == 27)
    assert(tree.query(3, 4).get == 16)
  }

  "modify for the index 2" should "update the leaf and upper folded values" in {
    assert(tree.modify(2, 1))
    assert(tree.query(0, 0).get == 2)
    assert(tree.query(1, 1).get == 8)
    assert(tree.query(2, 2).get == 1)
    assert(tree.query(3, 3).get == 7)
    assert(tree.query(4, 4).get == 9)
    assert(tree.query(5, 5).get == 11)
    assert(tree.query(0, 1).get == 10)
    assert(tree.query(0, 2).get == 11)
    assert(tree.query(0, 5).get == 38)
    assert(tree.query(3, 5).get == 27)
    assert(tree.query(3, 4).get == 16)
  }

  "modify for the index 3" should "update the leaf and upper folded values" in {
    assert(tree.modify(3, 4))
    assert(tree.query(0, 0).get == 2)
    assert(tree.query(1, 1).get == 8)
    assert(tree.query(2, 2).get == 1)
    assert(tree.query(3, 3).get == 4)
    assert(tree.query(4, 4).get == 9)
    assert(tree.query(5, 5).get == 11)
    assert(tree.query(0, 1).get == 10)
    assert(tree.query(0, 2).get == 11)
    assert(tree.query(0, 5).get == 35)
    assert(tree.query(3, 5).get == 24)
    assert(tree.query(3, 4).get == 13)
  }

  "modify for the index 4" should "update the leaf and upper folded values" in {
    assert(tree.modify(4, 2))
    assert(tree.query(0, 0).get == 2)
    assert(tree.query(1, 1).get == 8)
    assert(tree.query(2, 2).get == 1)
    assert(tree.query(3, 3).get == 4)
    assert(tree.query(4, 4).get == 2)
    assert(tree.query(5, 5).get == 11)
    assert(tree.query(0, 1).get == 10)
    assert(tree.query(0, 2).get == 11)
    assert(tree.query(0, 5).get == 28)
    assert(tree.query(3, 5).get == 17)
    assert(tree.query(3, 4).get == 6)
  }

  "modify for the index 5" should "update the leaf and upper folded values" in {
    assert(tree.modify(5, 7))
    assert(tree.query(0, 0).get == 2)
    assert(tree.query(1, 1).get == 8)
    assert(tree.query(2, 2).get == 1)
    assert(tree.query(3, 3).get == 4)
    assert(tree.query(4, 4).get == 2)
    assert(tree.query(5, 5).get == 7)
    assert(tree.query(0, 1).get == 10)
    assert(tree.query(0, 2).get == 11)
    assert(tree.query(0, 5).get == 24)
    assert(tree.query(3, 5).get == 13)
    assert(tree.query(3, 4).get == 6)
  }

  "modify for the index -1" should "be false and not make any changes" in {
    assert(!tree.modify(-1, 100))
    assert(tree.query(0, 0).get == 2)
    assert(tree.query(1, 1).get == 8)
    assert(tree.query(2, 2).get == 1)
    assert(tree.query(3, 3).get == 4)
    assert(tree.query(4, 4).get == 2)
    assert(tree.query(5, 5).get == 7)
    assert(tree.query(0, 1).get == 10)
    assert(tree.query(0, 2).get == 11)
    assert(tree.query(0, 5).get == 24)
    assert(tree.query(3, 5).get == 13)
    assert(tree.query(3, 4).get == 6)
  }

  "modify for the index 6" should "be false and not make any changes" in {
    assert(!tree.modify(-1, 100))
    assert(tree.query(0, 0).get == 2)
    assert(tree.query(1, 1).get == 8)
    assert(tree.query(2, 2).get == 1)
    assert(tree.query(3, 3).get == 4)
    assert(tree.query(4, 4).get == 2)
    assert(tree.query(5, 5).get == 7)
    assert(tree.query(0, 1).get == 10)
    assert(tree.query(0, 2).get == 11)
    assert(tree.query(0, 5).get == 24)
    assert(tree.query(3, 5).get == 13)
    assert(tree.query(3, 4).get == 6)
  }
}
