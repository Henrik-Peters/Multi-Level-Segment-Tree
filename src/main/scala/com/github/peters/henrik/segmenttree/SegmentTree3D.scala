// ---------------------------------------------------------------------
// MIT License
// Copyright (c) 2018 Henrik Peters
// See LICENSE file in the project root for full license information.
// ---------------------------------------------------------------------
package com.github.peters.henrik.segmenttree

import TreeMonoid._

object SegmentTree3D {
  def fromTensor[T](tensor: Seq[Seq[Seq[T]]], monoid: Monoid[T]): SegmentTree3D[T] = {
    val leafs = tensor.map(matrix => SegmentTree2D.fromMatrix(matrix, monoid)).map(tree => tree.tree)
    val treeMonoid = liftMonoid {leafs.head.monoid} {leafs.head.rootRange}

    new SegmentTree3D(SegmentTree.buildTree {leafs} {treeMonoid}, monoid, treeMonoid)
  }
}

class SegmentTree3D[T](
  private val root: TreeNode[SegmentTree[SegmentTree[T]]],
  val monoid: Monoid[T],
  val treeMonoid: Monoid[SegmentTree[SegmentTree[T]]]) {

  type SegTree3D = SegmentTree[SegmentTree[SegmentTree[T]]]

  //Invariant is checked by the inner segment tree
  val tree: SegTree3D = new SegTree3D(root, treeMonoid)

  def query(zStart: Int, zEnd: Int)(yStart: Int, yEnd: Int)(xStart: Int, xEnd: Int): Option[T] = {
    query {Range(zStart, zEnd)} {Range(yStart, yEnd)} {Range(xStart, xEnd)}
  }

  def query(zRange: Range)(yRange: Range)(xRange: Range): Option[T] = {
    tree.query(zRange) match {
      case Some(yTree) =>

        yTree.query(yRange) match {
          case Some(xTree) => xTree.query(xRange)
          case None => None
        }

      case None => None
    }
  }

  def modify(z: Int)(y: Int)(x: Int)(newValue: T): Boolean = {
    tree.query(z, z) match {
      case Some(subTree) =>
        var subTreeModified = false

        subTree.query(y, y) match {
          case Some(leaf) =>
            subTreeModified = leaf.modify(x, newValue) && subTree.modify(y, leaf)

          case None => return false
        }

        subTreeModified && tree.modify(z, subTree)

      case None => false
    }
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case other: SegmentTree3D[T] =>
        tree.equals(other.tree) &&
        monoid.equals(other.monoid)

      case _ => false
    }
  }

  override def toString: String = root.toString
}
