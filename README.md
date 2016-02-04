Effective Scala with Project Euler
==================================
Solutions to Project Euler problems with explanations of the Scala implementations.
Unit tests demonstrate and benchmark the problem solving algorithms.
Effective Scala is quick, efficient, and understandable.

Goal: solve the problems efficiently, using Scala effectively.
By-products: A library of utilities for solving mathematical problems; examples of the practical
applications of mathematics.

How to use this project:
------------------------
Visit the project euler web site, find a problem that interests you, solve it or try to solve it,
write a benchmark test against one of mine, and submit it as a push request. I'll gladly deprecate
a solution and leave it as a less-than ideal approach. For an example, see problem_10.

Solved Problems with Commentary:
--------------------------------
1-17, 19-26, 28-31, 34-41, 45, 48, 55, 56, 69, 74, 92, 97

Utility classes with unit tests:
--------------------------------
* Pascal's Triangle
* Big Square Root
* Sieve of Atkin
* Permutation Generator
* Spiral Number Grid
* Undirected Graph
* Unit Fraction
* CoinCollection, CoinConstraints, CoinCombination - generate combinations with restrictions
* NumberString - convert from digits to alphanumeric spellings and back

Who this project is for:
------------------------
* Intermediate Scala users; you've read Martin Odersky's _Programming in Scala_, worked through
  the examples and are hungry for more.
* You're interested in functional programming, or looking to expand or transition from procedural
  or quasi object-oriented programming paradigms.
* You may be working on your first real application in Scala.
* You're concerned that some of Scala's syntactic sugar and conveniences may cause performance
  problems when implementing non-trivial algorithms.
* You'd like to improve your math skills and see if abstract concepts can give you practical ideas.

History:
--------
I began working on the Project Euler problems in October 2009, with Scala 2.7 and I've updated the
code to be compliant with each major release. As Scala versions are promoted to release status, I
will update the sources as necessary, and whenever possible change solutions to illustrate and
leverage new functionality, e.g. the Scala 2.9.0 release brought the parallel collections
functionality and that has been used in the recent solutions (e.g. # 74, 92).
Now updated to: Scala 2.11.4

Related Links:
--------------
[Project Euler]<http://projecteuler.net>
