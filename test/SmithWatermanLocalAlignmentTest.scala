package org.biosequenceanalysis.tests

import junit.{Before, Test}
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import BackPointer._

class SmithWatermanLocalAlignmentTest extends AssertionsForJUnit {
  private var nwLocalAlignment:SmithWatermanLocalAlignment = _

    @Before def setup() {
      val rowSequence = List("P", "A", "W" ,"H", "E", "A", "E")
      val colSequence = List("H", "E", "A", "G", "A", "W", "G", "H", "E", "E")
      val blosum50Matrix = MatrixReader.fromFile("src/BLOSUM50.matrix");
      nwLocalAlignment = new SmithWatermanLocalAlignment(rowSequence, colSequence, blosum50Matrix, 8)
    }

    @Test def shouldReturnZeroesForTopRowAndLeftColumn() {
      assertEquals((0, None), nwLocalAlignment.score(0,0))
      assertEquals((0, None), nwLocalAlignment.score(0,10))
      assertEquals((0, None), nwLocalAlignment.score(7,0))
    }

    @Test def shouldReturnScoreForRow1Col1() {
      assertEquals((0, None), nwLocalAlignment.score(1,1))
    }

    @Test def shouldComputeOptimalAlignment() {
      val result = nwLocalAlignment.find
      val score = result._1
      val alignments = result._2
      assertEquals(28, score)
      assertEquals(List("A", "W", "-", "H", "E"),
                  alignments._1)
      assertEquals(List("A", "W", "G", "H", "E"),
                  alignments._2)
    }
}