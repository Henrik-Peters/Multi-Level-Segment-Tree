// ---------------------------------------------------------------------
// MIT License
// Copyright (c) 2018 Henrik Peters
// See LICENSE file in the project root for full license information.
// ---------------------------------------------------------------------
package com.github.peters.henrik.segmenttree

object TreeMonoid {

  /**
    * Lifts the monoid of T to a monoid for segment trees. Segment trees
    * are folded by applying the monoid for the type T to their leafs.
    */
  def liftMonoid[T]: Monoid[T] => Range => Monoid[SegmentTree[T]] = monoid => range => {
    TreeMonoid(range, monoid)
  }
}

case class TreeMonoid[T](range: Range, monoid: Monoid[T]) extends Monoid[SegmentTree[T]] {

  override def fold(a: SegmentTree[T], b: SegmentTree[T]): SegmentTree[T] = {
    SegmentTree.combineTrees(a, b, monoid).get
  }

  lazy val neutralTree: SegmentTree[T] = SegmentTree.fromSequence(
    Seq.fill {range.end - range.start + 1} {monoid.identity}, monoid)

  override def identity: SegmentTree[T] = neutralTree
}
