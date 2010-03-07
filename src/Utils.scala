package org.biosequenceanalysis

object Utils {
  def MIN_VALUE() : Int = { return -99999999 }

  // Given a list of numbers returns the max along with its position in the list
  def max(numberList : List[Int], maxValue : Int, currentPosition: Int, maxPosition : Int) : Tuple2[Int, Int] = numberList match {
     case Nil => (maxValue, maxPosition)
     case _ => if (numberList.head > maxValue) max(numberList.tail, numberList.head, currentPosition+1, currentPosition)
               else max(numberList.tail, maxValue, currentPosition+1, maxPosition)
  }
}