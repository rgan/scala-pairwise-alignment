package org.biosequenceanalysis.tests

import junit.{Before, Test}
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import BackPointer._

class NeedlemanWunschGlobalAlignmentTest extends AssertionsForJUnit {
    private var nwGlobalAlignment:NeedlemanWunschGlobalAlignment = _
  
    @Before def setup() {
      val rowSequence = List("P", "A", "W" ,"H", "E", "A", "E")
      val colSequence = List("H", "E", "A", "G", "A", "W", "G", "H", "E", "E")
      val blosum50Matrix = MatrixReader.fromFile("src/BLOSUM50.matrix");
      nwGlobalAlignment = new NeedlemanWunschGlobalAlignment(rowSequence, colSequence, blosum50Matrix, 8)
    }

    @Test def shouldReturnMultipleOfGapPenaltyForTopRowAndLeftColumn() {
      assertEquals((0, Top), nwGlobalAlignment.score(0,0))
      assertEquals((-8, Left), nwGlobalAlignment.score(0,1))
      assertEquals((-16, Left), nwGlobalAlignment.score(0,2))
      assertEquals((-8, Top ), nwGlobalAlignment.score(1,0))
      assertEquals((-80, Left), nwGlobalAlignment.score(0,10))
      assertEquals((-56, Top), nwGlobalAlignment.score(7,0))
    }

    @Test def shouldReturnScoreForRow1Col1() {
      assertEquals((-2, Diagonal), nwGlobalAlignment.score(1,1))
    }

    @Test def shouldComputeOptimalAlignment() {
      val result = nwGlobalAlignment.find
      val score = result._1
      val alignments = result._2
      assertEquals(1, score)
      assertEquals(List("-", "-", "P", "-", "A", "W", "-", "H", "E", "A", "E"),
                  alignments._1)
      assertEquals(List("H", "E", "A", "G", "A", "W", "G", "H", "E", "-", "E"),
                  alignments._2)
    }
}

