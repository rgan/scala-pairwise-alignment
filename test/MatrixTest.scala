package org.biosequenceanalysis.tests

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import org.junit.Assert._

class MatrixTest extends AssertionsForJUnit {

    @Test def shouldParseSequence() {
       val sequence = MatrixReader.parseSequence("A  R N D C Q E G H I L K M F P S T W Y V")
       assertEquals(20, sequence.size)
    }
  
    @Test def shouldReadScoreMatrixFromFile() {
      val blosum50Matrix = MatrixReader.fromFile("src/BLOSUM50.matrix");
      assertEquals(20, blosum50Matrix.noRows)
      assertEquals(20, blosum50Matrix.noCols)
      assertEquals(5, blosum50Matrix.valueFor("A","A"))
      assertEquals(5, blosum50Matrix.valueFor("V","V"))
    }
}