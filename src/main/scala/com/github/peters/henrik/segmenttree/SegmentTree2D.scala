// ---------------------------------------------------------------------
// MIT License
// Copyright (c) 2018 Henrik Peters
// See LICENSE file in the project root for full license information.
// ---------------------------------------------------------------------
package com.github.peters.henrik.segmenttree

import scala.language.postfixOps

/**
  * Factory singleton for creating two-dimensional segment trees.
  * Trees are built from two-dimensional data collections with monoids.
  */
object SegmentTree2D {
  def fromMatrix[T](matrix: Seq[Seq[T]], monoid: Monoid[T]): SegmentTree2D[T] = {
    val leafs = matrix.map(seq => seqToTree {seq} {monoid})
    val treeMonoid = liftMonoid {monoid} {leafs.head.rootRange}

    new SegmentTree2D(SegmentTree.buildTree {leafs} {treeMonoid}, monoid, treeMonoid)
  }

  private def seqToTree[T]: Seq[T] => Monoid[T] => SegmentTree[T] = seq => monoid => {
    SegmentTree.fromSequence(seq, monoid)
  }

  /**
    * Lifts the monoid of T to a monoid for segment trees. Segment trees
    * are folded by applying the monoid for the type T to their leafs.
    */
  def liftMonoid[T]: Monoid[T] => Range => Monoid[SegmentTree[T]] = monoid => range => {
    class TreeMonoid extends Monoid[SegmentTree[T]] {

      override def fold(a: SegmentTree[T], b: SegmentTree[T]): SegmentTree[T] = {
        SegmentTree.combineTrees(a, b, monoid).get
      }

      lazy val neutralTree: SegmentTree[T] = SegmentTree.fromSequence(
        Seq.fill {range.end - range.start + 1} {monoid.identity}, monoid)

      override def identity: SegmentTree[T] = neutralTree
    }

    new TreeMonoid
  }
}

/**
  * Defines the two-dimensional segment tree, which is constructed
  * from the companion object.
  *
  * @param root The top level node of the tree
  * @param monoid Used to fold the final elements in the tree
  * @param treeMonoid Used to fold the segment trees
  * @tparam T Type of the final elements stored in the tree
  */
class SegmentTree2D[T](private val root: TreeNode[SegmentTree[T]], val monoid: Monoid[T], val treeMonoid: Monoid[SegmentTree[T]]) {
  import SegmentTree2D._
  type SegTree2D = SegmentTree[SegmentTree[T]]

  //Invariant is checked by the inner segment tree
  val tree: SegTree2D = new SegmentTree[SegmentTree[T]](root, treeMonoid)

  def query(yStart: Int, yEnd: Int)(xStart: Int, xEnd: Int): Option[T] = {
    query {Range(yStart, yEnd)} {Range(xStart, xEnd)}
  }

  def query(yRange: Range)(xRange: Range): Option[T] = {
    tree.query(yRange) match {
      case Some(subTree) => subTree.query(xRange)
      case None => None
    }
  }

  def modifyLeaf(y: Int)(row: Seq[T]): Boolean = {
    tree.modify(y, seqToTree {row} {monoid})
  }

  def modify(y: Int)(x: Int)(newValue: T): Boolean = {
    tree.query(y, y) match {
      case Some(leaf) => leaf.modify(x, newValue) && tree.modify(y, leaf)
      case None => false
    }
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case other: SegmentTree2D[T] =>
        tree.equals(other.tree) &&
        monoid.equals(other.monoid)

      case _ => false
    }
  }
}
