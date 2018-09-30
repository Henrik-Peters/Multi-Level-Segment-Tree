// ---------------------------------------------------------------------
// MIT License
// Copyright (c) 2018 Henrik Peters
// See LICENSE file in the project root for full license information.
// ---------------------------------------------------------------------
package com.github.peters.henrik.segmenttree

/**
  * A monoid is an algebraic structure used for folding.
  * Every binary function with an identity element is a
  * monoid when it satifies the following conditions:
  *
  * 1.) fold can be applied for every element of type T<br>
  * 2.) fold is left and right associative, which means:<br>
  *
  *     <strong>fold(fold(a, b), c) = fold(a, fold(b, c))</strong><br>
  *     for any arbitrary element a,b,c of type T<br>
  *
  * 3.) There exists an identity element such that:<br>
  *
  *     <strong>fold(a, identity) = fold(identity, a) = a</strong><br>
  *     for any arbitrary element a of type T
  *
  * @tparam T Type of the monoid
  */
trait Monoid[T] {
  def fold(a: T, b: T): T
  def identity: T
}
