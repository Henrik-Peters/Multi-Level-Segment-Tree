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

  val data = Array(1, 3, 5, 7, 9, 11)
  val tree = new SegmentTree(data, IntegerAddition)

  "query for the complete interval" should "be the root value" in {
    assert(tree.query(0, 5).get == 36)
  }

  "query for a left exact matching interval" should "be the node value" in {
    assert(tree.query(0, 1).get == 4)
  }

  "query for another left exact matching interval" should "be the node value" in {
    assert(tree.query(0, 2).get == 9)
  }

  "query for a right exact matching interval" should "be the node value" in {
    assert(tree.query(3, 4).get == 16)
  }

  "query for another right exact matching interval" should "be the node value" in {
    assert(tree.query(3, 5).get == 27)
  }

  "query for two left different nodes" should "be the folded value" in {
    assert(tree.query(1, 2).get == 8)
  }

  "query for two right different nodes" should "be the folded value" in {
    assert(tree.query(4, 5).get == 20)
  }

  "query for an interval that splits over the root node" should "be the folded value" in {
    assert(tree.query(2, 3).get == 12)
  }

  "query for an interval that splits more over the root node" should "be the folded value" in {
    assert(tree.query(1, 4).get == 24)
  }
}
