package org.biosequenceanalysis

import BackPointer._

class SmithWatermanLocalAlignment(sequence1 : List[String], sequence2 : List[String], substitutionMatrix: Matrix[Int], gapPenalty : Int) {
  val this.sequence1 = sequence1
  val this.sequence2 = sequence2
  val this.gapPenalty = gapPenalty

  // all entries in the matrix are initialized to tuple: (0, None)
  val matrix = new Matrix("X" :: this.sequence1, "X" :: this.sequence2, (0, None))

  // computes the score with backpointer for a given i,j
  def score(row : Int, col: Int) : Tuple2[Int, BackPointer] = {
    if (row == 0 || col == 0) return matrix.getValueAt(row, col)
    val diagonal: Int = matrix.getValueAt(row - 1, col - 1)._1 + substitutionMatrix.valueFor(sequence1(row - 1), sequence2(col - 1))
    val left: Int = matrix.getValueAt(row, col - 1)._1 - gapPenalty
    val top: Int = matrix.getValueAt(row - 1, col)._1 - gapPenalty
    val tuple = Utils.max(diagonal :: left :: top :: (0 :: Nil), Utils.MIN_VALUE, 0, 0)
    return (tuple._1, tuple._2 match {
      case 0 => Diagonal
      case 1 => Left
      case 2 => Top
      case 3 => None
    })
  }

  // returns optimal alignment found along with the score
  def find() : Tuple2[Int, Tuple2[List[String], List[String]]] = {
     var maxScore = Utils.MIN_VALUE
     var maxScoreLocation: Tuple2[Int, Int] = (-1,-1)
     for(i <- 1 to matrix.noRows-1) {
        for(j <- 1 to matrix.noCols-1) {
          val scoreWithBackPointer = score(i, j)
          matrix.setValueAt(i, j, scoreWithBackPointer)
          if (scoreWithBackPointer._1 > maxScore) {
             maxScore = scoreWithBackPointer._1
             maxScoreLocation = (i, j)
          }
      }
     }
     return (maxScore,
             traceback(maxScoreLocation._1, maxScoreLocation._2, Nil, Nil))
  }

  // returns optimal alignment using the stored backpointers
  def traceback(i : Int, j : Int, seq1 : List[String], seq2 :List[String]) : Tuple2[List[String], List[String]] =  {
    if (matrix.getValueAt(i,j)._1 == 0) {
      return (seq1, seq2)
    }
    matrix.getValueAt(i,j)._2 match {
      case Diagonal => traceback(i-1,j-1, matrix.rowSequenceValueAt(i) :: seq1, matrix.columnSequenceValueAt(j) :: seq2)
      case Left => traceback(i, j-1, "-" :: seq1, matrix.columnSequenceValueAt(j) :: seq2)
      case Top => traceback(i-1, j, matrix.rowSequenceValueAt(i) :: seq1, "-" :: seq2 )
    }
  }
  
}