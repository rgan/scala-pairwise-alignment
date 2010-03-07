package org.biosequenceanalysis.tests

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._

class UtilsTest extends AssertionsForJUnit {

  @Test def shouldReturnMaxValue() {
    val result: Tuple2[Int, Int] = Utils.max(List(3, 1, 2), 0, 0, 0)
    assertEquals(3, result._1)
  }

  @Test def shouldReturnPositionOfMaxValue() {
    assertEquals(0, Utils.max(List(3, 1, 2), 0, 0, 0)._2)
    assertEquals(1, Utils.max(List(1, 3, 2), 0, 0, 0)._2)
    assertEquals(2, Utils.max(List(1, 2, 3), 0, 0, 0)._2)
  }

  @Test def shouldReturnInputMaxValueAndPositionForEmptyList() {
    assertEquals(0, Utils.max(List(), 0, 0, 0)._1)
    assertEquals(0, Utils.max(List(), 0, 0, 0)._2)
  }
}