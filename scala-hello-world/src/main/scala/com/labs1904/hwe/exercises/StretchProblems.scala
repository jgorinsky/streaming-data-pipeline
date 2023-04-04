package com.labs1904.hwe.exercises

object StretchProblems {

  /*
  Checks if a string is palindrome.
 */
  def isPalindrome(s: String): Boolean = {
    s.reverse.equals(s)
  }

  /*
For a given number, return the next largest number that can be created by rearranging that number's digits.
If no larger number can be created, return -1
 */
  def getNextBiggestNumber(i: Integer): Int = {
     i.toString.split("")
       .permutations
       .map(s => s.mkString.toInt)
       .filter(n => n > i)
       .toList
       .sorted
       .headOption
       .getOrElse(-1)
  }

}
