package com.digitalasset.daml.lf
package data

import java.math.{MathContext, RoundingMode, BigDecimal => JBigDec}

import scala.collection.immutable.Set


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

  def add(addend: DamlBigDecimal, augend: DamlBigDecimal): DamlBigDecimal = {
    val newFlags = addend.dbdFlags union augend.dbdFlags
    if (newFlags contains Invalid)
      DamlBigDecimal(JBigDec.ONE, newFlags)
    else
      DamlBigDecimal(addend.bigDecimal.add(augend.bigDecimal), newFlags)
  }

  def subtract(minuend: DamlBigDecimal, subtrahend: DamlBigDecimal): DamlBigDecimal = {
    val newFlags = minuend.dbdFlags union subtrahend.dbdFlags
    if (newFlags contains Invalid)
      DamlBigDecimal(JBigDec.ONE, newFlags)
    else
      DamlBigDecimal(minuend.bigDecimal.subtract(subtrahend.bigDecimal), newFlags)
  }

  def multiply(multiplier: DamlBigDecimal, multiplicand: DamlBigDecimal): DamlBigDecimal = {
    val newFlags = multiplier.dbdFlags union multiplicand.dbdFlags
    if (newFlags contains Invalid)
      DamlBigDecimal(JBigDec.ONE, newFlags)
    else
      DamlBigDecimal(multiplier.bigDecimal.multiply(multiplicand.bigDecimal), newFlags)
    // We should be checking for overflow and underflow here, although given the scale that is unlikely.
  }

  def pow(base: DamlBigDecimal, exponent: Long): DamlBigDecimal = {
    if (base.dbdFlags contains Invalid)
      base
    else if (exponent < 0)
      DamlBigDecimal(JBigDec.ONE, Set.empty + Invalid)
    else if (exponent == 0)
      one
    else if (exponent <= Integer.MAX_VALUE)
      DamlBigDecimal(base.bigDecimal.pow(exponent.toInt), base.dbdFlags)
    else
      // We should be able to handle this with x^(ab+c) == (x^a)^b * x^c, however I don't have time
      // for that right now, and will need to come back to it.
      DamlBigDecimal(JBigDec.ONE, Set.empty + Invalid)

  }

  def divide(maxPrecision: Long, roundingMode: String, numerator: DamlBigDecimal, denominator: DamlBigDecimal): DamlBigDecimal = {
    val newFlags = numerator.dbdFlags union denominator.dbdFlags
    if (newFlags contains Invalid)
      DamlBigDecimal(JBigDec.ONE, newFlags)
    else if (denominator.bigDecimal == JBigDec.ZERO)
      DamlBigDecimal(JBigDec.ONE, newFlags + Invalid)
    else if (numerator.bigDecimal == JBigDec.ZERO)
      DamlBigDecimal(JBigDec.ZERO, newFlags)
    else if (maxPrecision > Integer.MAX_VALUE || maxPrecision < 1)
      DamlBigDecimal(JBigDec.ONE, newFlags + Invalid)
    else
      try {
        val mc = new MathContext(maxPrecision.toInt, RoundingMode.valueOf(roundingMode))
        val d = numerator.bigDecimal.divide(denominator.bigDecimal, mc)
        if (d == JBigDec.ZERO)
          DamlBigDecimal(d, newFlags + Underflow)
        else
          DamlBigDecimal(d, newFlags)
      } catch {
        case _: ArithmeticException =>
          DamlBigDecimal(JBigDec.ONE, Set.empty + Invalid)
      }
  }

  def divMod(maxPrecision: Long, roundingMode: String, numerator: DamlBigDecimal, denominator: DamlBigDecimal): (DamlBigDecimal, DamlBigDecimal) = {
    val newFlags = numerator.dbdFlags union denominator.dbdFlags
    if (newFlags contains Invalid)
      (DamlBigDecimal(JBigDec.ONE, newFlags), DamlBigDecimal(JBigDec.ONE, newFlags))
    else if (denominator.bigDecimal == JBigDec.ZERO)
      (DamlBigDecimal(JBigDec.ONE, newFlags + Invalid), DamlBigDecimal(JBigDec.ONE, newFlags + Invalid))
    else if (numerator.bigDecimal == JBigDec.ZERO)
      (DamlBigDecimal(JBigDec.ZERO, newFlags), DamlBigDecimal(JBigDec.ZERO, newFlags))
    else if (maxPrecision > Integer.MAX_VALUE || maxPrecision < 1)
      (DamlBigDecimal(JBigDec.ONE, newFlags + Invalid), DamlBigDecimal(JBigDec.ONE, newFlags + Invalid))
    else
      try {
        val mc = new MathContext(maxPrecision.toInt, RoundingMode.valueOf(roundingMode))
        val d = numerator.bigDecimal.divideAndRemainder(denominator.bigDecimal, mc)
        (DamlBigDecimal(d(0), newFlags), DamlBigDecimal(d(1), newFlags))
      } catch {
        case _: ArithmeticException =>
          (DamlBigDecimal(JBigDec.ONE, newFlags + Invalid), DamlBigDecimal(JBigDec.ONE, newFlags + Invalid))
      }
  }

  def compare(lhs: DamlBigDecimal, rhs: DamlBigDecimal): Int =
    (lhs.dbdFlags contains Invalid, rhs.dbdFlags contains Invalid) match {
      case (true, true) => 0
      case (true, false) => -1
      case (false, true) => 1
      case (false, false) => lhs.bigDecimal compareTo rhs.bigDecimal
    }

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
