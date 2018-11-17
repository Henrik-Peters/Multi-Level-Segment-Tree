# Multi-Level-Segment-Tree
Segment trees are used to perform range queries and modifications in logarithmic time.
This implementation can be used with any arbitrary element type and a suitable monoid.
The concept of the segment tree can be applied to higher dimensions. This project also
includes the implementation of multidimensional segment trees.

## Range queries
For a given array A[0 ... n-1] with n elements we call a range query a function which
transforms a slice of A into a single value. This is achieved by applying a given binary
combining operation recursively for the slice of A. For example:

```scala
val A = Array(1, 3, 7, 9, 11)
val range = Range(1, 3)

def sum(a: Int, b: Int): Int = a + b
val identity = 0

val result = rangeQuery(A, range, sum, identity)
assert(result equals 19)
```

The first concept in the range query function is to transform A into the slice of A,
in this example (3, 7, 9). The final step is to apply the binary combiner for creating the
final result. In the example the final result is: sum(3, sum(7, 9)) = sum(3, 16) = 19

## Performance
The simplest way to perform range queries is with a for loop iteration. The for loop variant
is very easy to implement and requires no additional memory. The disadvantage is the linear
time complexity. Another solution is to precalculate and store all possible range queries.
For *n* as the total number of elements in A:

| Operation | Iteration | Segment tree |  Precalculated   |
|-----------|:---------:|:------------:|:----------------:|
|  Memory   |    -      |    O(*n*)    |O(*n<sup>2</sup>*)|
|  Build    |    -      |    O(*n*)    |O(*n<sup>2</sup>*)|
|  Query    |  O(*n*)   |  O(log *n*)  |      O(1)        |
|  Modify   |   O(1)    |  O(log *n*)  |     O(*n*)       |

## Building
Maven is used as Build-Tool. Maven will also execute Scalatest and Scaladoc:

```bash
mvn package
```
Additionally a jar with dependencies is created. To use this project as a dependency:
```bash
mvn clean install
```

## Visualization
The data structure can be visualized with the dump function. The dump will generate a graph
and png file with the `Graphviz`-Tool. The dump function is only available for one-dimensional
trees. For example:

Sum of integers  | Substrings
:---------------------------------------------------------------------------------------------------------------------------:|:---------------------------------------------------------------------------------------------------------------------------------------:
![Sumtree-Example](https://raw.githubusercontent.com/wiki/Henrik-Peters/Multi-Level-Segment-Tree/images/sumtree-example.png) | ![Substringtree-Example](https://raw.githubusercontent.com/wiki/Henrik-Peters/Multi-Level-Segment-Tree/images/substringtree-example.png)

## Parameterizing trees with monoids
To construct a new segment tree you also have to specify a monoid for the new tree.
This makes the implementation more general because it can be adopted for different use cases.
There are many monoids and basically each monoid is used for a different use case. Just to give
a few examples of monoids and their use cases with segment trees:

**Query:** Sum of integers in the given range
```scala
object IntegerAddition extends Monoid[Int] {
  def fold(a: Int, b: Int): Int = a + b
  def identity: Int = 0
}
```

**Query:** Substring of the given range
```scala
object StringConcatenation extends Monoid[String] {
  def fold(a: String, b: String): String = a concat b
  def identity: String = ""
}
```

**Query:** Biggest integer in the given range
```scala
object IntegerMax extends Monoid[Int] {
  def fold(a: Int, b: Int): Int = a max b
  def identity: Int = Integer.MIN_VALUE
}
```

**Query:** Slice of the given range
```scala
object ListConcat extends Monoid[List[_]] {
  def fold(a: List[_], b: List[_]): List[_] = a ::: b
  def identity: List[_] = List.empty
}
```

## Extending segment trees to higher dimensions
Segment trees can contain any type. This type can also be a segment tree, in theory this
recursion is already the construction of a multi-dimensional tree. The monoid for the outer
tree has to fold segment trees. But this monoid can be constructed automatically from the inner
monoid, by folding a tree element-wise. It still makes sense to provide different types for
multi-dimensional trees to achieve more type safety and better readability. There is some kind
of code duplication for the different dimensions, but abstracting the logic just for n dimensions
is not very type safe and can be confusing for lower dimensions.
