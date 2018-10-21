// ---------------------------------------------------------------------
// MIT License
// Copyright (c) 2018 Henrik Peters
// See LICENSE file in the project root for full license information.
// ---------------------------------------------------------------------
package com.github.peters.henrik.segmenttree

import scala.language.postfixOps

/**
  * Defines the two-dimensional segment tree, which is constructed from
  * a sequence of sequences. Sequences should have the same length.
  *
  * @param matrix Construct the tree from this multi sequence
  * @param monoid Used to fold the final elements in the tree
  * @tparam T Type of the final elements stored in the tree
  */
class SegmentTree2D[T](val matrix: Seq[Seq[T]], val monoid: Monoid[T]) {
  type SegTree2D = SegmentTree[SegmentTree[T]]

  lazy val xRange: Int = matrix.head length
  lazy val yRange: Int = matrix length

  val mapSeqToTree: Seq[T] => SegmentTree[T] = seq => {
    new SegmentTree[T](seq, monoid)
  }

  val mapMatrixToSeqOfTrees: Seq[Seq[T]] => Seq[SegmentTree[T]] = seq => {
    seq map mapSeqToTree
  }

  val mapMatrixToTree: Seq[Seq[T]] => SegTree2D = seq => {
    new SegTree2D (mapMatrixToSeqOfTrees {seq}, TreeMonoid)
  }

  val foldSequences: Seq[T] => Seq[T] => Seq[T] = a => b => {
    a zip b map {pair => monoid.fold(pair._1, pair._2)}
  }

  //The invariant is checked by the wrapped segment tree
  val tree: SegTree2D = mapMatrixToTree(matrix)

  override def equals(obj: Any): Boolean = {
    obj match {
      case tree: SegmentTree2D[T] =>
        matrix.equals(tree.matrix) &&
          monoid.equals(tree.monoid)

      case _ => false
    }
  }

  /**
    * Lifts the monoid of T to a monoid for segment trees. Segment trees
    * are folded by applying the monoid for the type T to their leafs.
    */
  object TreeMonoid extends Monoid[SegmentTree[T]] {

    override def fold(a: SegmentTree[T], b: SegmentTree[T]): SegmentTree[T] = {
      new SegmentTree[T](foldSequences (a.data) (b.data), monoid)
    }

    override def identity: SegmentTree[T] = {
      new SegmentTree[T](Seq.fill (xRange) (monoid.identity), monoid)
    }
  }
}
