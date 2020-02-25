package com.digitalasset.daml.lf
package data

import java.math.{BigDecimal => JBigDec}
import java.math.RoundingMode


/* a DAML BigDecimal carries a Scala BigDecimal and a set of flags indicating
  some properties of its value.
  DAML BigDecimal numbers are of arbitrary precision, but are by design not
  serialisable. They can, however, be converted into DAML `Numeric n` if
  their size and scale does not exceed the limits of `Numeric n`.
 */
case class DamlBigDecimal(bigDecimal: JBigDec, dbdFlags: Set[DBDFlag]) {

  def toNumeric(scale: Int, roundingMode: String): Either[String, Numeric] =
    if (dbdFlags contains Invalid) // if already invalid, report it
      Left(s"Converting an invalid numeric value $scale, $roundingMode")
    else
      Numeric.checkForOverflow(bigDecimal.setScale(scale, RoundingMode.valueOf(roundingMode)))

  override def toString: String =
    if (dbdFlags contains Invalid) "<invalid value>"
    else bigDecimal.toString
}

object DamlBigDecimal {
  def fromNumeric(x: Numeric): DamlBigDecimal = DamlBigDecimal(x, Set.empty)

  val zero: DamlBigDecimal = DamlBigDecimal(JBigDec.ZERO, Set.empty)

  val one: DamlBigDecimal = DamlBigDecimal(JBigDec.ONE, Set.empty)
}


sealed trait DBDFlag

// Number was subject to rounding before
case object Inexact extends DBDFlag

// Number resulted from an illegal operations (e.g., 1 / 0)
// or was computed using an already-invalid number
case object Invalid extends DBDFlag

// Number is too small to represent (but not actually zero)
case object Underflow extends DBDFlag
