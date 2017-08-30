import forcomp.Anagrams.{Occurrences, Word}

import scala.collection.immutable.SortedMap

def wordOccurrences(w: Word): Occurrences = w groupBy (_.toLower) map { case (c, s) => (c, s.size) } toList

wordOccurrences("Kkkkkylllle")

List("a", "b").foldLeft("")((a: Word, b: Word) => a concat b)

val l = List(('a', 2), ('b', 2))

for {
  (c, i) <- l
  n <- 1 to i
} yield (c, n)

List(('a', 2), ('b', 2))

val x = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
val y = List(('r', 1))

def subtract(x: Occurrences, y: Occurrences): Occurrences = {
  (y foldLeft SortedMap[Char, Int]() ++ x) { case (map, (c, i)) => {
    val j = map(c) - i
    if (j == 0) map - c
    else map updated (c, j)
  } } toList
}