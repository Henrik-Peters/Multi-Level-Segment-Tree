// ---------------------------------------------------------------------
// MIT License
// Copyright (c) 2018 Henrik Peters
// See LICENSE file in the project root for full license information.
// ---------------------------------------------------------------------
package com.github.peters.henrik.segmenttree

/**
  * Defines the one-dimensional segment tree, which is constructed
  * from the data array. Range queries take logarithmic time.
  *
  * @param data Construct the tree from this data
  * @param monoid Used to fold elements in the tree
  * @tparam T Type of the elements stored in the tree
  */
class SegmentTree[T](data: Array[T], monoid: Monoid[T]) {

  private val root: TreeNode = buildTree(0, data.length - 1)
  private val rootRange = Range(0, data.length - 1)

  abstract class TreeNode {
    var value: T
  }

  case class Node(
     range: Range,
     override var value: T,
     left: TreeNode,
     right: TreeNode
  ) extends TreeNode

  case class Leaf(index: Int, override var value: T) extends TreeNode


  private def mid(left: Int, right: Int) = (left + right) / 2

  def buildTree(left: Int, right: Int): TreeNode = {
    if (left != right) {
      val node = Node(Range(left, right), monoid.identity,
        buildTree(left, mid(left, right)),
        buildTree(mid(left, right) + 1, right))

      node.value = monoid.fold(node.left.value, node.right.value)
      node
    }
    else
      Leaf(left, data(left))
  }

  def query(left: Int, right: Int): Option[T] = {
    query(Range(left, right))
  }

  def query(range: Range): Option[T] = {
    if (rootRange.contains(range))
      Some(queryNode(range, root))
    else
      None
  }

  private def queryNode(range: Range, node: TreeNode): T = {
    node match {
      case Node(subRange, value, leftChild, rightChild) =>
        if (range.contains(subRange)) {
          value

        } else if (range.disjoint(subRange)) {
          monoid.identity

        } else {
          monoid.fold(
            queryNode(range, leftChild),
            queryNode(range, rightChild))
        }

      case Leaf(index, value) =>
        if (range.contains(index)) {
          value
        } else {
          monoid.identity
        }
    }
  }

}
