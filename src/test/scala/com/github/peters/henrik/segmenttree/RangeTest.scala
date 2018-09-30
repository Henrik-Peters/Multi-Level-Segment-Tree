// ---------------------------------------------------------------------
// MIT License
// Copyright (c) 2018 Henrik Peters
// See LICENSE file in the project root for full license information.
// ---------------------------------------------------------------------
package com.github.peters.henrik.segmenttree

import org.scalatest.{FlatSpec, Matchers}

class RangeTest extends FlatSpec with Matchers {

  "A Range" should "contain inner numbers" in {
    val range = Range(4, 7)
    range.contains(5) should be (true)
    range.contains(6) should be (true)
  }

  "A Range" should "not contain outer numbers" in {
    val range = Range(4, 7)
    range.contains(2) should not be true
    range.contains(3) should not be true
    range.contains(8) should not be true
    range.contains(9) should not be true
  }

  "A Range" should "contain the inclusive left border" in {
    val range = Range(4, 7)
    range.contains(4) should be (true)
  }

  "A Range" should "contain the inclusive right border" in {
    val range = Range(4, 7)
    range.contains(7) should be (true)
  }

  "A Range" should "contain itself" in {
    val rangeA = Range(4, 7)
    val rangeB = Range(4, 7)
    rangeA.contains(rangeB) should be (true)
    rangeB.contains(rangeA) should be (true)
  }

  "A Range" should "contain a subrange with common left border" in {
    val rangeA = Range(4, 7)
    val rangeB = Range(4, 6)
    rangeA.contains(rangeB) should be (true)
    rangeB.contains(rangeA) should be (false)
  }

  "A Range" should "contain a subrange which is the left border" in {
    val rangeA = Range(4, 7)
    val rangeB = Range(4, 4)
    rangeA.contains(rangeB) should be (true)
    rangeB.contains(rangeA) should be (false)
  }

  "A Range" should "contain a subrange with common right border" in {
    val rangeA = Range(4, 7)
    val rangeB = Range(6, 7)
    rangeA.contains(rangeB) should be (true)
    rangeB.contains(rangeA) should be (false)
  }

  "A Range" should "contain a subrange which is the right border" in {
    val rangeA = Range(4, 7)
    val rangeB = Range(7, 7)
    rangeA.contains(rangeB) should be (true)
    rangeB.contains(rangeA) should be (false)
  }

  "A Range" should "not contain a larger range" in {
    val rangeA = Range(4, 7)
    val rangeB = Range(2, 10)
    rangeA.contains(rangeB) should be (false)
    rangeB.contains(rangeA) should be (true)
  }

  "A Range" should "not contain a different range" in {
    val rangeA = Range(4, 7)
    val rangeB = Range(10, 20)
    rangeA.contains(rangeB) should be (false)
    rangeB.contains(rangeA) should be (false)
  }

  "A Range" should "not contain the next element of the border" in {
    val rangeA = Range(1, 3)
    val rangeB = Range(4, 6)
    val rangeC = Range(7, 9)
    rangeA.contains(rangeB) should be (false)
    rangeA.contains(rangeC) should be (false)
    rangeB.contains(rangeA) should be (false)
    rangeB.contains(rangeC) should be (false)
    rangeC.contains(rangeA) should be (false)
    rangeC.contains(rangeB) should be (false)
  }

  "A Range" should "overlap with itself" in {
    val rangeA = Range(10, 20)
    val rangeB = Range(10, 20)
    rangeA.overlaps(rangeB) should be (true)
    rangeB.overlaps(rangeA) should be (true)
  }

  "A Range" should "overlap with the left border" in {
    val rangeA = Range(10, 20)
    val rangeB = Range(5, 15)
    rangeA.overlaps(rangeB) should be (true)
    rangeB.overlaps(rangeA) should be (true)
  }

  "A Range" should "overlap with the right border" in {
    val rangeA = Range(10, 20)
    val rangeB = Range(15, 25)
    rangeA.overlaps(rangeB) should be (true)
    rangeB.overlaps(rangeA) should be (true)
  }

  "A Range" should "overlap with the inclusive left border" in {
    val rangeA = Range(10, 20)
    val rangeB = Range(5, 10)
    rangeA.overlaps(rangeB) should be (true)
    rangeB.overlaps(rangeA) should be (true)
  }

  "A Range" should "overlap with the inclusive right border" in {
    val rangeA = Range(10, 20)
    val rangeB = Range(20, 25)
    rangeA.overlaps(rangeB) should be (true)
    rangeB.overlaps(rangeA) should be (true)
  }

  "A Range" should "not overlap with the next number of the left border" in {
    val rangeA = Range(10, 20)
    val rangeB = Range(5, 9)
    rangeA.overlaps(rangeB) should be (false)
    rangeB.overlaps(rangeA) should be (false)
  }

  "A Range" should "not overlap with the next number of the right border" in {
    val rangeA = Range(10, 20)
    val rangeB = Range(21, 25)
    rangeA.overlaps(rangeB) should be (false)
    rangeB.overlaps(rangeA) should be (false)
  }

  "A Range" should "overlap with a range in between" in {
    val rangeA = Range(10, 20)
    val rangeB = Range(13, 17)
    rangeA.overlaps(rangeB) should be (true)
    rangeB.overlaps(rangeA) should be (true)
  }

  "A Range" should "be disjointed with a lower range" in {
    val rangeA = Range(5, 10)
    val rangeB = Range(1, 3)
    rangeA.disjoint(rangeB) should be (true)
    rangeB.disjoint(rangeA) should be (true)
  }

  "A Range" should "be disjointed with a higher range" in {
    val rangeA = Range(5, 10)
    val rangeB = Range(16, 19)
    rangeA.disjoint(rangeB) should be (true)
    rangeB.disjoint(rangeA) should be (true)
  }

  "A Range" should "be disjointed with the next number of a lower range" in {
    val rangeA = Range(5, 10)
    val rangeB = Range(1, 4)
    rangeA.disjoint(rangeB) should be (true)
    rangeB.disjoint(rangeA) should be (true)
  }

  "A Range" should "be disjointed with the next number of a higher range" in {
    val rangeA = Range(5, 10)
    val rangeB = Range(11, 19)
    rangeA.disjoint(rangeB) should be (true)
    rangeB.disjoint(rangeA) should be (true)
  }

  "A Range" should "not be disjointed with itself" in {
    val rangeA = Range(5, 10)
    val rangeB = Range(5, 10)
    rangeA.disjoint(rangeB) should be (false)
    rangeB.disjoint(rangeA) should be (false)
  }

  "A Range" should "not be disjointed with an inner range" in {
    val rangeA = Range(5, 10)
    val rangeB = Range(7, 8)
    rangeA.disjoint(rangeB) should be (false)
    rangeB.disjoint(rangeA) should be (false)
  }

  "A Range" should "not be disjointed with a left common border range" in {
    val rangeA = Range(5, 10)
    val rangeB = Range(2, 5)
    rangeA.disjoint(rangeB) should be (false)
    rangeB.disjoint(rangeA) should be (false)
  }

  "A Range" should "not be disjointed with a right common border range" in {
    val rangeA = Range(5, 10)
    val rangeB = Range(10, 16)
    rangeA.disjoint(rangeB) should be (false)
    rangeB.disjoint(rangeA) should be (false)
  }
}
