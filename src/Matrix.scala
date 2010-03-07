package org.biosequenceanalysis

class Matrix[T](rowSequence :List[String], columnSequence :List[String], initValue : T) {
  private val this.rowSequence = rowSequence
  private val this.columnSequence = columnSequence
  private val values = new Array[Array[T]](noRows,noCols)

  for(i <- 0 to noRows-1) {
      for(j <- 0 to noCols-1) {
          values(i)(j) = initValue
      }
  }

  def valueFor(sRow : String, sCol : String) : T = {
    return values(indexFor(sRow, true))(indexFor(sCol, false))
  }

  def setValueAt(row : Int, col : Int, value : T) : Unit = {
     values(row)(col) = value
  }

  def getValueAt(row : Int, col : Int) : T = {
     return values(row)(col)
  }

  def noRows() : Int = {
     return this.rowSequence.length
  }

  def rowSequenceValueAt(i : Int) : String = {
    return this.rowSequence(i)
  }

  def columnSequenceValueAt(i : Int) : String = {
    return this.columnSequence(i)
  }

  def noCols() : Int = {
    return this.columnSequence.length
  }

  private def indexFor(s : String, row : Boolean) : Int = {
     return if (row) rowSequence.indexOf(s) else columnSequence.indexOf(s)
  }
}