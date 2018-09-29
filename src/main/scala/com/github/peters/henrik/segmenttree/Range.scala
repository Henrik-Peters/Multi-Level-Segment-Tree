// ---------------------------------------------------------------------
// MIT License
// Copyright (c) 2018 Henrik Peters
// See LICENSE file in the project root for full license information.
// ---------------------------------------------------------------------
package com.github.peters.henrik.segmenttree

/**
  * Defines an inclusive range with [start, end].
  * Start must be less or equal than end.
  *
  * @param start The left range bound (inclusive)
  * @param end The right range bound (inclusive)
  */
class Range(start: Int, end: Int) {
  require(start <= end)

  def contains(x: Int): Boolean = {
    (x >= start) && (x <= end)
  }

  def contains(other: Range): Boolean = {
    (start <= other.start) && (end >= other.end)
  }

  def overlaps(other: Range): Boolean = {
    (start >= other.start && start <= other.end) ||
    (end >= other.end && end <= other.end)
  }

  def disjoint(other: Range): Boolean = {
    (end < other.start) || (start > other.end)
  }
}
