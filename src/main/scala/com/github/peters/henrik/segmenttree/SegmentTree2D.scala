// ---------------------------------------------------------------------
// MIT License
// Copyright (c) 2018 Henrik Peters
// See LICENSE file in the project root for full license information.
// ---------------------------------------------------------------------
package com.github.peters.henrik.segmenttree

/**
  * Defines the two-dimensional segment tree, which is constructed from
  * a sequence of sequences. Sequences should have the same length.
  *
  * @param data Construct the tree from this multi sequence
  * @param monoid Used to fold the final elements in the tree
  * @tparam T Type of the final elements stored in the tree
  */
class SegmentTree2D[T](data: Seq[Seq[T]], monoid: Monoid[T]) {
  type SegTree2D = SegmentTree[SegmentTree[T]]

}
