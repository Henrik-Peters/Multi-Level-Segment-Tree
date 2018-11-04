// ---------------------------------------------------------------------
// MIT License
// Copyright (c) 2018 Henrik Peters
// See LICENSE file in the project root for full license information.
// ---------------------------------------------------------------------
package com.github.peters.henrik.segmenttree

import scala.sys.process._
import java.io.{File, PrintWriter}

/**
  * Factory singleton for creating one-dimensional segment trees.
  * Trees are built from data collections with monoids.
  */
object SegmentTree {

  def fromSequence[T](data: Seq[T], monoid: Monoid[T]): SegmentTree[T] = {
    new SegmentTree(buildTree {data} {monoid}, monoid)
  }

  def combineTrees[T](a: SegmentTree[T], b: SegmentTree[T], monoid: Monoid[T]): Option[SegmentTree[T]] = {
    def build: TreeNode[T] => TreeNode[T] => TreeNode[T] = fstTree => sndTree => {
      (fstTree, sndTree) match {

        case (fstNode: Node[T], sndNode: Node[T]) =>
          Node[T](fstNode.range,
            monoid.fold(fstNode.value, sndNode.value),
            build {fstNode.left} {sndNode.left},
            build {fstNode.right} {sndNode.right})

        case (fstLeaf: Leaf[T], sndLeaf: Leaf[T]) =>
          Leaf(fstLeaf.index, monoid.fold(fstLeaf.value, sndLeaf.value))

        case _ => null
      }
    }

    if (a.rootRange == b.rootRange) {
      Some(new SegmentTree(build {a.root} {b.root}, monoid))
    } else None
  }

  private def mid(left: Int, right: Int) = (left + right) / 2

  private[segmenttree] def buildTree[T]: Seq[T] => Monoid[T] => TreeNode[T] = {
    seq => monoid => {
      def build: Int => Int => TreeNode[T] = left => right => {
        if (left != right) {
          val node = Node[T](Range(left, right), monoid.identity,
            build         (left)         (mid(left, right)),
            build (mid(left, right) + 1)      (right))

          node.value = monoid.fold(node.left.value, node.right.value)
          node
        }
        else
          Leaf(left, seq(left))
      }

      build (0) (seq.length - 1)
    }
  }
}

abstract class TreeNode[T] {
  var value: T
}

case class Node[T](
  range: Range,
  override var value: T,
  left: TreeNode[T],
  right: TreeNode[T]
) extends TreeNode[T]

case class Leaf[T](index: Int, override var value: T) extends TreeNode[T]

/**
  * Defines the one-dimensional segment tree, which is constructed
  * from the companion object. Range queries take logarithmic time.
  *
  * @param root The top level node of the tree
  * @param monoid Used to fold elements in the tree
  * @tparam T Type of the elements stored in the tree
  */
class SegmentTree[T](private val root: TreeNode[T], val monoid: Monoid[T]) {
  import SegmentTree._
  assert(invariant())

  private[segmenttree] lazy val rootRange: Range = {
    root match {
      case Node(range, _, _, _) => range
      case Leaf(_, _) => Range(0, 0)
    }
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

  private def queryNode(range: Range, node: TreeNode[T]): T = {
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

  def modify(index: Int, newValue: T): Boolean = {
    val modified = modify(index, newValue, root)
    assert(invariant())
    modified
  }

  private def modify(index: Int, newValue: T, node: TreeNode[T]): Boolean = {
    node match {
      case Node(segment, _, left, right) if segment.contains(index) =>
        val modified = modify(index, newValue, left) || modify(index, newValue, right)
        node.value = monoid.fold(left.value, right.value)
        modified

      case Leaf(idx, _) if index == idx =>
        node.value = newValue
        true

      case _ => false
    }
  }

  private def invariant(node: TreeNode[T] = root): Boolean = {
    node match {
      case Node(segment, value, left, right) =>
        val invFold = value.equals(monoid.fold(left.value, right.value))

        val invLeftSegment = left match {
          case Node(leftSegment, _, _, _) =>
            leftSegment.start == segment.start &&
              leftSegment.end == mid(segment.start, segment.end)

          case Leaf(index, _) =>
            segment.start == index
        }

        val invRightSegment = right match {
          case Node(rightSegment, _, _, _) =>
            rightSegment.start == mid(segment.start, segment.end) + 1 &&
              rightSegment.end == segment.end

          case Leaf(index, _) =>
            segment.end == index
        }

        invFold && invLeftSegment && invRightSegment &&
        invariant(left) && invariant(right)

      case Leaf(_, _) =>
        true
    }
  }

  def dumpTree(dumpName: String): Unit = {
    import language.postfixOps

    "mkdir -p dump" !
    val writer = new PrintWriter(new File("dump/" + dumpName + ".gv"))

    //Add all nodes to a list ordered by the depth and then by key
    class DumpMetaData(val key: Int, val depth: Int)
    var nodeList : List[(DumpMetaData,TreeNode[T])] = List()

    object KeyGenerator {
      var key : Int = 0
      def getNextKey : Int = {key += 1; key}
    }

    def addToNodeList(node: TreeNode[T], depth: Int = 0): Unit = {
      import KeyGenerator.getNextKey

      node match {
        case Node(_, _, leftChild, rightChild) =>
          nodeList = (new DumpMetaData(getNextKey, depth), node) :: nodeList
          addToNodeList(leftChild, depth + 1)
          addToNodeList(rightChild, depth + 1)

        case Leaf(_, _) =>
          nodeList = (new DumpMetaData(getNextKey, depth), node) :: nodeList
      }
    }

    def getKeyByNode(node: TreeNode[T]): Option[Int] = {
      nodeList find {_._2 == node} match {
        case Some((metaData, _)) => Some(metaData.key)
        case None => None
      }
    }

    addToNodeList(root)

    //Sort by depth then key, so the left child will always come first
    nodeList = nodeList.sortWith((a, b) => {
      a._1.depth < b._1.depth || (a._1.depth == b._1.depth && a._1.key < b._1.key)
    })

    writer.write("digraph G {\n")
    writer.write("node [style=filled, fontname = \"arial\"];\n")
    writer.write("graph [pad=\"0.1\", nodesep=\"1\", ranksep=\"1.5\"];\n")

    //Create the edges from the node keys
    nodeList foreach {node =>
      node._2 match {
        case Node(_, _, leftChild, rightChild) =>
          writer.write(node._1.key + " -> " + getKeyByNode(leftChild).get + ";\n")
          writer.write(node._1.key + " -> " + getKeyByNode(rightChild).get + ";\n")

        case Leaf(_, _) =>
      }
    }

    //Node format and text
    nodeList foreach {node =>
      node._2 match {
        case Node(segment, value, _, _) =>
          writer.write(node._1.key +
            "[shape=record, fillcolor=\"#E2E2E2\", style=filled," +
            "label=\"{" + value + "|[" + segment.start + "," + segment.end + "]}\"];\n")

        case Leaf(index, value) =>
          writer.write(node._1.key +
            "[shape=record, fillcolor=\"#EEC591\", style=filled," +
            "label=\"{" + value + "|[" + index + "]}\"];\n")
      }
    }

    writer.write("}\n")
    writer.close()

    "dot -Tpng dump/" + dumpName + ".gv -o dump/" + dumpName + ".png" !;
    "xdg-open dump/" + dumpName + ".png" !;
    "rm dump/" + dumpName + ".gv" !
  }
}
