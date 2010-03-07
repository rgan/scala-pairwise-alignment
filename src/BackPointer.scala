package org.biosequenceanalysis

object BackPointer extends Enumeration {
  type BackPointer = Value
  val Diagonal, Left, Top, None = Value
}
