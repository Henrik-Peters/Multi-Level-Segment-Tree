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
