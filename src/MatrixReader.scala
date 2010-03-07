package org.biosequenceanalysis

import io.Source

object MatrixReader {

  def fromFile(filePath : String) : Matrix[Int] = {
     val lines : List[String] = Source.fromFile(filePath).getLines.toList
     val rowSequence = parseSequence(lines.head)
     val colSequence = parseSequence(lines.tail.head)
     val matrix = new Matrix(rowSequence, colSequence, 0)
     var row = 0
     for(line <- lines.tail.tail) {
       val rowValues = parseSequence(line)
       var col = 0
       for(v <- rowValues){
          matrix.setValueAt(row, col, rowValues(col).toInt)
          col = col + 1
       }
       row = row + 1
    }
    return matrix
  }

  def parseSequence(input : String) : List[String] = {
    return input.split("\\s+").toList
  }
}