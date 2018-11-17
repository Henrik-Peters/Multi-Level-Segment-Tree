// ---------------------------------------------------------------------
// MIT License
// Copyright (c) 2018 Henrik Peters
// See LICENSE file in the project root for full license information.
// ---------------------------------------------------------------------
package com.github.peters.henrik.segmenttree

/**
  * Example usage for the segment tree to create substrings in logarithmic time.
  * All possible substrings of hello world will be queried from the segment tree.
  */
object App {
  def main(args: Array[String]): Unit = {

    object StringConcatenation extends Monoid[String] {
      def fold(a: String, b: String): String = a concat b
      def identity: String = ""
    }

    val hello = "Hello World!".toCharArray map {_.toString}
    val range = Range(0, hello.length - 1)
    val subranges = Range.subranges(range)
    val tree = SegmentTree.fromSequence(hello, StringConcatenation)

    println(s"All substrings of ${hello.mkString} from the segment tree:")

    subranges flatMap {range => tree.query(range)} foreach println
  }
}
