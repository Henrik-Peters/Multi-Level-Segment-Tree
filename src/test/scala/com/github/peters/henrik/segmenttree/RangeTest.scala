// ---------------------------------------------------------------------
// MIT License
// Copyright (c) 2018 Henrik Peters
// See LICENSE file in the project root for full license information.
// ---------------------------------------------------------------------
package com.github.peters.henrik.segmenttree

import org.scalatest.{FlatSpec, Matchers}

class RangeTest extends FlatSpec with Matchers {

  "A Range" should "contain inner numbers" in {
    val range = new Range(4, 7)
    range.contains(5) should be (true)
    range.contains(6) should be (true)
  }

  "A Range" should "not contain outer numbers" in {
    val range = new Range(4, 7)
    range.contains(2) should not be true
    range.contains(3) should not be true
    range.contains(8) should not be true
    range.contains(9) should not be true
  }

  "A Range" should "contain the inclusive left border" in {
    val range = new Range(4, 7)
    range.contains(4) should be (true)
  }

  "A Range" should "contain the inclusive right border" in {
    val range = new Range(4, 7)
    range.contains(7) should be (true)
  }

  "A Range" should "contain itself" in {
    val rangeA = new Range(4, 7)
    val rangeB = new Range(4, 7)
    rangeA.contains(rangeB) should be (true)
    rangeB.contains(rangeA) should be (true)
  }

  "A Range" should "contain a subrange with common left border" in {
    val rangeA = new Range(4, 7)
    val rangeB = new Range(4, 6)
    rangeA.contains(rangeB) should be (true)
    rangeB.contains(rangeA) should be (false)
  }

  "A Range" should "contain a subrange which is the left border" in {
    val rangeA = new Range(4, 7)
    val rangeB = new Range(4, 4)
    rangeA.contains(rangeB) should be (true)
    rangeB.contains(rangeA) should be (false)
  }

  "A Range" should "contain a subrange with common right border" in {
    val rangeA = new Range(4, 7)
    val rangeB = new Range(6, 7)
    rangeA.contains(rangeB) should be (true)
    rangeB.contains(rangeA) should be (false)
  }

  "A Range" should "contain a subrange which is the right border" in {
    val rangeA = new Range(4, 7)
    val rangeB = new Range(7, 7)
    rangeA.contains(rangeB) should be (true)
    rangeB.contains(rangeA) should be (false)
  }

  "A Range" should "not contain a larger range" in {
    val rangeA = new Range(4, 7)
    val rangeB = new Range(2, 10)
    rangeA.contains(rangeB) should be (false)
    rangeB.contains(rangeA) should be (true)
  }

  "A Range" should "not contain a different range" in {
    val rangeA = new Range(4, 7)
    val rangeB = new Range(10, 20)
    rangeA.contains(rangeB) should be (false)
    rangeB.contains(rangeA) should be (false)
  }

  "A Range" should "not contain the next element of the border" in {
    val rangeA = new Range(1, 3)
    val rangeB = new Range(4, 6)
    val rangeC = new Range(7, 9)
    rangeA.contains(rangeB) should be (false)
    rangeA.contains(rangeC) should be (false)
    rangeB.contains(rangeA) should be (false)
    rangeB.contains(rangeC) should be (false)
    rangeC.contains(rangeA) should be (false)
    rangeC.contains(rangeB) should be (false)
  }

  "A Range" should "overlap with itself" in {
    val rangeA = new Range(10, 20)
    val rangeB = new Range(10, 20)
    rangeA.overlaps(rangeB) should be (true)
    rangeB.overlaps(rangeA) should be (true)
  }

}
