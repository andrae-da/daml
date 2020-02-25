package com.digitalasset.daml.lf
package data


/* a DAML BigDecimal carries a Scala BigDecimal and a set of flags indicating
  some properties of its value.
  DAML BigDecimal numbers are of arbitrary precision, but are by design not
  serialisable. They can, however, be converted into DAML `Numeric n` if
  their size and scale does not exceed the limits of `Numeric n`.
 */
case class DamlBigDecimal(scalaBigDec: BigDecimal, dbdFlags: Set[DBDFlag]) {

  def toNumeric(scale: Int, rmode: String): Either[String, Numeric] =
    if (dbdFlags contains Invalid) // if already invalid, report it
      Left(s"Converting an invalid numeric value $scale, $rmode")
    else // if valid, check size
      Numeric.checkForOverflow(scalaBigDec.bigDecimal)

  override def toString: String =
    if (dbdFlags contains Invalid) "<invalid value>"
    else scalaBigDec.toString
}

object DamlBigDecimal {
  def fromNumeric(x: Numeric): DamlBigDecimal = DamlBigDecimal(x, Set.empty)

  val zero: DamlBigDecimal = DamlBigDecimal(BigDecimal(0), Set.empty)

  val one: DamlBigDecimal = DamlBigDecimal(BigDecimal(1), Set.empty)
}


sealed trait DBDFlag

// Number was subject to rounding before
case object Inexact extends DBDFlag

// Number resulted from an illegal operations (e.g., 1 / 0)
// or was computed using an already-invalid number
case object Invalid extends DBDFlag

// Number is too small to represent (but not actually zero)
case object Underflow extends DBDFlag
