Some Scala Idioms
=================

One of the great things about Scala is the language has evolved to become more user friendly.
Although sometimes it's just syntactic sugar--it is still sweet nonetheless.


adding up a list of digits, used to be:

digits.foldLeft (0) (_ + _)

now:

digits.sum


get the last element of a list, used to be:

col((col.size) - 1) 

now:

col.last

More to come!