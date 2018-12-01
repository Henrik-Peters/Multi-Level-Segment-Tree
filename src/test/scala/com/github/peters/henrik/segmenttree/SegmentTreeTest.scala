// ---------------------------------------------------------------------
// MIT License
// Copyright (c) 2018 Henrik Peters
// See LICENSE file in the project root for full license information.
// ---------------------------------------------------------------------
package com.github.peters.henrik.segmenttree

import Range._
import scala.util.Random
import org.scalatest.{FlatSpec, Matchers}

class SegmentTreeTest extends FlatSpec with Matchers {

  object IntegerAddition extends Monoid[Int] {
    def fold(a: Int, b: Int): Int = a + b
    def identity: Int = 0
  }

  object IntegerMultiplication extends Monoid[Int] {
    def fold(a: Int, b: Int): Int = a * b
    def identity: Int = 1
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

  "Combining trees which are just leafs" should "be the combined result leaf" in {
    val fstTree = SegmentTree.fromSequence(Seq(1), IntegerAddition)
    val sndTree = SegmentTree.fromSequence(Seq(2), IntegerAddition)
    val result = SegmentTree.combineTrees(fstTree, sndTree, IntegerAddition).get

    assert(result.query(0, 0).get == 3)
  }

  "Combining trees with 2 elements" should "be the combined result tree" in {
    val fstTree = SegmentTree.fromSequence(Seq(1, 2), IntegerAddition)
    val sndTree = SegmentTree.fromSequence(Seq(10, 11), IntegerAddition)
    val result = SegmentTree.combineTrees(fstTree, sndTree, IntegerAddition).get

    assert(result.query(0, 0).get == 11)
    assert(result.query(1, 1).get == 13)
    assert(result.query(0, 1).get == 24)
  }

  "Combining trees with 3 elements" should "be the combined result tree" in {
    val fstTree = SegmentTree.fromSequence(Seq(1, 2, 3), IntegerAddition)
    val sndTree = SegmentTree.fromSequence(Seq(4, 5, 6), IntegerAddition)
    val result = SegmentTree.combineTrees(fstTree, sndTree, IntegerAddition).get

    assert(result.query(0, 0).get == 5)
    assert(result.query(1, 1).get == 7)
    assert(result.query(2, 2).get == 9)
    assert(result.query(0, 2).get == 21)
    assert(result.query(0, 1).get == 12)
  }

  "Combining trees with 4 elements" should "be the combined result tree" in {
    val fstTree = SegmentTree.fromSequence(Seq(1, 2, 3, 4), IntegerAddition)
    val sndTree = SegmentTree.fromSequence(Seq(11, 12, 13, 14), IntegerAddition)
    val result = SegmentTree.combineTrees(fstTree, sndTree, IntegerAddition).get

    assert(result.query(0, 0).get == 12)
    assert(result.query(1, 1).get == 14)
    assert(result.query(2, 2).get == 16)
    assert(result.query(3, 3).get == 18)
    assert(result.query(0, 1).get == 26)
    assert(result.query(0, 3).get == 60)
    assert(result.query(2, 3).get == 34)
  }

  "Combining trees with 5 elements" should "be the combined result tree" in {
    val fstTree = SegmentTree.fromSequence(Seq(1, 2, 3, 4, 5), IntegerAddition)
    val sndTree = SegmentTree.fromSequence(Seq(21, 22, 23, 24, 25), IntegerAddition)
    val result = SegmentTree.combineTrees(fstTree, sndTree, IntegerAddition).get

    assert(result.query(0, 0).get == 22)
    assert(result.query(1, 1).get == 24)
    assert(result.query(2, 2).get == 26)
    assert(result.query(3, 3).get == 28)
    assert(result.query(4, 4).get == 30)
    assert(result.query(0, 1).get == 46)
    assert(result.query(0, 2).get == 72)
    assert(result.query(0, 4).get == 130)
    assert(result.query(3, 4).get == 58)
  }

  "Combining trees with different element amount" should "be none" in {
    val fstTree = SegmentTree.fromSequence(Seq(1, 2, 3), IntegerAddition)
    val sndTree = SegmentTree.fromSequence(Seq(4, 5, 6, 7, 8), IntegerAddition)
    val result = SegmentTree.combineTrees(fstTree, sndTree, IntegerAddition)

    assert(result.isEmpty)
  }

  "Combining a leaf with 2 elements" should "be none" in {
    val fstTree = SegmentTree.fromSequence(Seq(1), IntegerAddition)
    val sndTree = SegmentTree.fromSequence(Seq(2, 3), IntegerAddition)
    val result = SegmentTree.combineTrees(fstTree, sndTree, IntegerAddition)

    assert(result.isEmpty)
  }

  "Equals for two identical leafs" should "be true" in {
    val fstTree = SegmentTree.fromSequence(Seq(1), IntegerAddition)
    val sndTree = SegmentTree.fromSequence(Seq(1), IntegerAddition)
    assert(fstTree.equals(sndTree))
    assert(sndTree.equals(fstTree))
  }

  "Equals for two identical leafs with different monoids" should "be false" in {
    val fstTree = SegmentTree.fromSequence(Seq(1), IntegerAddition)
    val sndTree = SegmentTree.fromSequence(Seq(1), IntegerMultiplication)
    assert(!fstTree.equals(sndTree))
    assert(!sndTree.equals(fstTree))
  }

  "Equals for two identical trees with 2 elements" should "be true" in {
    val fstTree = SegmentTree.fromSequence(Seq(1, 2), IntegerAddition)
    val sndTree = SegmentTree.fromSequence(Seq(1, 2), IntegerAddition)
    assert(fstTree.equals(sndTree))
    assert(sndTree.equals(fstTree))
  }

  "Equals for two identical trees with 3 elements" should "be true" in {
    val fstTree = SegmentTree.fromSequence(Seq(1, 2, 3), IntegerAddition)
    val sndTree = SegmentTree.fromSequence(Seq(1, 2, 3), IntegerAddition)
    assert(fstTree.equals(sndTree))
    assert(sndTree.equals(fstTree))
  }

  "Equals for two identical trees with 4 elements" should "be true" in {
    val fstTree = SegmentTree.fromSequence(Seq(5, 6, 7, 8), IntegerAddition)
    val sndTree = SegmentTree.fromSequence(Seq(5, 6, 7, 8), IntegerAddition)
    assert(fstTree.equals(sndTree))
    assert(sndTree.equals(fstTree))
  }

  "Equals for two identical trees with 5 elements" should "be true" in {
    val fstTree = SegmentTree.fromSequence(Seq(5, 11, 2, 13, 4), IntegerAddition)
    val sndTree = SegmentTree.fromSequence(Seq(5, 11, 2, 13, 4), IntegerAddition)
    assert(fstTree.equals(sndTree))
    assert(sndTree.equals(fstTree))
  }

  "Equals for two different trees with 2 elements" should "be false" in {
    val fstTree = SegmentTree.fromSequence(Seq(5, 11), IntegerAddition)
    val sndTree = SegmentTree.fromSequence(Seq(5), IntegerAddition)
    assert(!fstTree.equals(sndTree))
    assert(!sndTree.equals(fstTree))
  }

  "Equals for two different trees with 3 elements" should "be false" in {
    val fstTree = SegmentTree.fromSequence(Seq(5, 11, 13), IntegerAddition)
    val sndTree = SegmentTree.fromSequence(Seq(5, 11), IntegerAddition)
    assert(!fstTree.equals(sndTree))
    assert(!sndTree.equals(fstTree))
  }

  "Equals for two different trees with a lot of elements" should "be false" in {
    val fstTree = SegmentTree.fromSequence(Seq(5, 11, 13, 17, 21, 23), IntegerAddition)
    val sndTree = SegmentTree.fromSequence(Seq(5, 11, 13), IntegerAddition)
    assert(!fstTree.equals(sndTree))
    assert(!sndTree.equals(fstTree))
  }

  "Equals for two different trees with the same number of elements" should "be false" in {
    val fstTree = SegmentTree.fromSequence(Seq(5, 11, 12), IntegerAddition)
    val sndTree = SegmentTree.fromSequence(Seq(5, 11, 13), IntegerAddition)
    assert(!fstTree.equals(sndTree))
    assert(!sndTree.equals(fstTree))
  }

  "Equals for two different but symmetric trees" should "be false" in {
    val fstTree = SegmentTree.fromSequence(Seq(2, 3), IntegerAddition)
    val sndTree = SegmentTree.fromSequence(Seq(3, 2), IntegerAddition)
    assert(!fstTree.equals(sndTree))
    assert(!sndTree.equals(fstTree))
  }

  class LoopQuery(sequence: Seq[Int], monoid: Monoid[Int]){
    val array: Array[Int] = sequence.toArray

    def query(range: Range): Option[Int] = {
      try {
        var accumulator = monoid.identity

        for (x <- range.start to range.end) {
          accumulator = monoid.fold(accumulator, array(x))
        }

        Some(accumulator)
      } catch {
        case _: Throwable => None
      }
    }

    def modify(x: Int)(newValue: Int): Boolean = {
      try {
        array(x) = newValue
        true
      } catch {
        case _: Throwable => false
      }
    }

  }

  val test: Seq[Int] => Unit = sequence => {
    val loopQuery = new LoopQuery(sequence, IntegerAddition)
    val tree: SegmentTree[Int] = SegmentTree.fromSequence(sequence, IntegerAddition)
    val length = sequence.length

    val valids = subranges(tree.rootRange)
    val elements = sequence.indices.toList
    val invalids = subranges(Range(-3, -1)) :::
      subranges(Range(length, length + 3)) :::
      List(Range(0, length), Range(0, length + 1), Range(0, length + 2)) :::
      List(Range(-1, length), Range(-2, length + 1), Range(-3, length + 2))

    def testQueries(): Unit = {
      valids.foreach(range => {
        val treeResult = tree.query(range)
        val loopResult = loopQuery.query(range)

        treeResult shouldEqual loopResult
        treeResult should not equal Option.empty
        loopResult should not equal Option.empty
      })

      invalids.foreach(range => {
        val treeResult = tree.query(range)
        val loopResult = loopQuery.query(range)

        treeResult shouldEqual loopResult
        treeResult shouldEqual Option.empty
        loopResult shouldEqual Option.empty
      })
    }

    testQueries()
    val modifyCycles = 3
    val random = new Random(sequence.head)

    for (_ <- 0 until modifyCycles) {
      elements.foreach(x => {
        val newValue = random.nextInt(300)
        val treeModify = tree.modify(x, newValue)
        val loopModify = loopQuery.modify(x)(newValue)

        treeModify shouldEqual loopModify
        treeModify shouldEqual true
        loopModify shouldEqual true
        testQueries()
      })
    }
  }

  "The loop implementation" should "match with the tree implementation" in {
    test{Seq(7)}
    test{Seq(3, 2)}
    test{Seq(5, 4, 1)}
    test{Seq(8, 7, 6, 2)}
    test{Seq(4, 1, 9, 8, 3)}
    test{Seq(9, 2, 3, 4, 5, 6)}
    test{Seq(1, 3, 7, 9, 4, 5, 3)}
    test{Seq(6, 8, 4, 3, 2, 1, 8, 3)}
    test{Seq(2, 7, 3, 9, 4, 5, 2, 3, 8)}
    test{Seq(7, 3, 2, 1, 9, 8, 4, 2, 1, 3, 7, 8, 3, 4, 2,
             1, 4, 5, 7, 8, 3, 1, 4, 5, 8, 6, 1, 3, 4, 7)}
  }
}
