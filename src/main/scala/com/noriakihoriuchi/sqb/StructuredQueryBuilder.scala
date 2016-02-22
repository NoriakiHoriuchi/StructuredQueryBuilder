package com.noriakihoriuchi.sqb

import org.joda.time.{DateTime, DateTimeZone}

object StructuredQueryBuilder {

  abstract class StructuredTerm(query: String) {
    def build = query

    override def toString = query

    def and(terms: StructuredTerm*): StructuredTerm = terms match {
      case Nil => this
      case first +: Nil if first == StructuredQuery.empty => this
      case _ => StructuredQueryBuilder.and(Nil, this +: terms)
    }
  }

  case class StructuredQuery(query: String) extends StructuredTerm(query)

  object StructuredQuery {
    val empty = StructuredQuery("")
  }

  case class StructuredOption(query: String) {
    override def toString = query
  }

  def and(options: Seq[StructuredOption] = Nil, terms: Seq[StructuredTerm]): StructuredTerm =
    operator("and", options, terms)

  def not(options: Seq[StructuredOption] = Nil, terms: Seq[StructuredTerm]): StructuredTerm =
    operator("not", options, terms)

  def or(options: Seq[StructuredOption] = Nil, terms: Seq[StructuredTerm]): StructuredTerm =
    operator("or", options, terms)

  def matchall = StructuredQuery("matchall")

  def near(options: Seq[StructuredOption] = Nil, terms: Seq[StructuredTerm]): StructuredTerm =
    operator("near", options, terms)

  def prefix(options: Seq[StructuredOption] = Nil, terms: Seq[StructuredTerm]): StructuredTerm =
    operator("prefix", options, terms)

  def phase(options: Seq[StructuredOption] = Nil, terms: Seq[StructuredTerm]): StructuredTerm =
    operator("phase", options, terms)

  def range[A](field: String, options: Seq[StructuredOption] = Nil, range: RangeValue[A]): StructuredTerm = {
    val optionsString = if (options.nonEmpty) {
      options.mkString(" ") + " "
    } else ""
    StructuredQuery(s"(range field=$field $optionsString${range.query})")
  }

  def term(options: Seq[StructuredOption] = Nil, terms: Seq[StructuredTerm]): StructuredTerm =
    operator("term", options, terms)

  private def operator(op: String, options: Seq[StructuredOption] = Nil, terms: Seq[StructuredTerm]): StructuredTerm = {
    val optionsString = if (options.nonEmpty) {
      options.mkString(" ") + " "
    } else ""
    val termsString = terms.mkString(" ")
    StructuredQuery(s"($op $optionsString$termsString)")
  }

  private def stringify[A](opt: Option[A]) = opt match {
    case Some(v: String) => s"'$v'"
    case Some(v: DateTime) => s"'${v.withZone(DateTimeZone.UTC).toString}'"
    case Some(v) => v.toString
    case None => ""
  }

  case class RangeValue[A](containsMin: Boolean, from: Option[A], to: Option[A], containsMax: Boolean) {
    val left = if (containsMin) "[" else "{"
    val fromStr = stringify(from)
    val toStr = stringify(to)
    val right = if (containsMax) "]" else "}"
    val query = s"$left$fromStr,$toStr$right"
  }

  def gtlt[A](from: Option[A], to: Option[A]): RangeValue[A] =
    RangeValue[A](containsMin = false, from, to, containsMax = false)

  def gelt[A](from: Option[A], to: Option[A]): RangeValue[A] =
    RangeValue[A](containsMin = true, from, to, containsMax = false)

  def gtle[A](from: Option[A], to: Option[A]): RangeValue[A] =
    RangeValue[A](containsMin = false, from, to, containsMax = true)

  def gele[A](from: Option[A], to: Option[A]): RangeValue[A] =
    RangeValue[A](containsMin = true, from, to, containsMax = true)

  def field(key: String): StructuredOption = StructuredOption(s"field=$key")

  def boost(value: Int): StructuredOption = StructuredOption(s"boost=$value")

  def distance(value: Int): StructuredOption = StructuredOption(s"distance=$value")

  object Implicits {
    implicit def stringValueTuple2ToField(tuple2: (String, String)): StructuredTerm =
      StructuredQuery(s"${tuple2._1}:'${tuple2._2}'")

    implicit def intValueTuple2ToField(tuple2: (String, Int)): StructuredTerm =
      StructuredQuery(s"${tuple2._1}:${tuple2._2}")

    implicit def doubleValueTuple2ToField(tuple2: (String, Double)): StructuredTerm =
      StructuredQuery(s"${tuple2._1}:${tuple2._2}")

    implicit def bigDecimalValueTuple2ToField(tuple2: (String, BigDecimal)): StructuredTerm =
      StructuredQuery(s"${tuple2._1}:${tuple2._2}")

    implicit def dateTimeValueTuple2ToField(tuple2: (String, DateTime)): StructuredTerm =
      StructuredQuery(s"${tuple2._1}:'${tuple2._2.withZone(DateTimeZone.UTC).toString}'")

    implicit def stringToStructuredTerm(value: String): StructuredTerm =
      StructuredQuery(s"'$value'")

    implicit def intToStructuredTerm(value: Int): StructuredTerm =
      StructuredQuery(s"$value")

    implicit def doubleToStructuredTerm(value: Double): StructuredTerm =
      StructuredQuery(s"$value")

    implicit def bigDecimalToStructuredTerm(value: BigDecimal): StructuredTerm =
      StructuredQuery(s"$value")

    implicit def dateTimeToStructuredTerm(value: DateTime): StructuredTerm =
      StructuredQuery(s"'${value.withZone(DateTimeZone.UTC).toString}'")
  }

}