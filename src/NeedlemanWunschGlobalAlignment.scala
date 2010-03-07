package org.biosequenceanalysis

import BackPointer._

class NeedlemanWunschGlobalAlignment(sequence1 : List[String], sequence2 : List[String], substitutionMatrix: Matrix[Int], gapPenalty : Int) {
  val this.sequence1 = sequence1
  val this.sequence2 = sequence2
  val this.gapPenalty = gapPenalty

  // matrix value is a tuple (score, backpointer)
  val matrix: Matrix[Tuple2[Int,BackPointer]] = new Matrix("X" :: this.sequence1, "X" :: this.sequence2, (0,None))

  // initialize top row F(0,j) to -i*gapPenalty
  for(j <- 0 to matrix.noCols-1) {
      matrix.setValueAt(0, j, (-1*j*gapPenalty, Left))
  }
  // initialize left column F(i, 0) to -j*gapPenalty
  for(i <- 0 to matrix.noRows-1) {
      matrix.setValueAt(i, 0, (-1*i*gapPenalty, Top))
  }

  // computes the score with backpointer for a given i,j
  def score(row : Int, col: Int) : Tuple2[Int, BackPointer] = {
    if (row == 0 || col == 0) return matrix.getValueAt(row, col)
    val diagonal: Int = (matrix.getValueAt(row - 1, col - 1))._1 + substitutionMatrix.valueFor(sequence1(row - 1), sequence2(col - 1))
    val left: Int = (matrix.getValueAt(row, col - 1))._1 - gapPenalty
    val top: Int = matrix.getValueAt(row - 1, col)._1 - gapPenalty
    val tuple = Utils.max(diagonal :: left :: (top :: Nil), Utils.MIN_VALUE, 0, 0)
    return (tuple._1, tuple._2 match {
      case 0 => Diagonal
      case 1 => Left
      case 2 => Top
    })
  }

  // returns optimal alignment found along with the score
  def find() : Tuple2[Int, Tuple2[List[String], List[String]]] = {
     for(i <- 1 to matrix.noRows-1) {
        for(j <- 1 to matrix.noCols-1) {
          matrix.setValueAt(i, j, score(i, j))
      }
     } 
     return (matrix.getValueAt(matrix.noRows-1, matrix.noCols-1)._1,
             traceback(matrix.noRows-1, matrix.noCols-1, Nil, Nil))
  }

  // returns optimal alignment using the stored backpointers
  def traceback(i : Int, j : Int, seq1 : List[String], seq2 :List[String]) : Tuple2[List[String], List[String]] =  {
    if (i == 0 && j == 0) {
      return (seq1, seq2)
    }
    (matrix.getValueAt(i, j))._2 match {
      case Diagonal => traceback(i-1,j-1, matrix.rowSequenceValueAt(i) :: seq1, matrix.columnSequenceValueAt(j) :: seq2)
      case Left => traceback(i, j-1, "-" :: seq1, matrix.columnSequenceValueAt(j) :: seq2)
      case Top => traceback(i-1, j, matrix.rowSequenceValueAt(i) :: seq1, "-" :: seq2 )
    }
  }
}