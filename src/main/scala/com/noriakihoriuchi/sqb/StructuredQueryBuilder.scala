package com.noriakihoriuchi.sqb

import org.joda.time.{DateTime, DateTimeZone}

object StructuredQueryBuilder {

  abstract class StructuredTerm(query: String) {
    def build = query

    override def toString = query

    def and(term: StructuredTerm): StructuredTerm = term match {
      case StructuredQuery.empty => this
      case _ => StructuredQueryBuilder.and(terms = Seq(this, term))
    }
  }

  case class StructuredQuery(query: String) extends StructuredTerm(query)

  object StructuredQuery {
    val empty = StructuredQuery("")
  }

  case class StructuredOption(query: String) {
    def :+(next: StructuredOption) = StructuredOption(s"$query ${next.query}")

    def +:(next: StructuredOption) = StructuredOption(s"$query ${next.query}")

    override def toString = query
  }

  object StructuredOption {
    val empty = StructuredOption("")
  }

  def and(option: StructuredOption = StructuredOption.empty, terms: Seq[StructuredTerm] = Nil): StructuredTerm =
    operator("and", option, terms)

  def not(option: StructuredOption = StructuredOption.empty, terms: Seq[StructuredTerm]): StructuredTerm =
    operator("not", option, terms)

  def or(option: StructuredOption = StructuredOption.empty, terms: Seq[StructuredTerm]): StructuredTerm =
    operator("or", option, terms)

  def matchall = StructuredQuery("matchall")

  def near(option: StructuredOption = StructuredOption.empty, terms: Seq[StructuredTerm]): StructuredTerm =
    operator("near", option, terms)

  def prefix(option: StructuredOption = StructuredOption.empty, terms: Seq[StructuredTerm]): StructuredTerm =
    operator("prefix", option, terms)

  def phase(option: StructuredOption = StructuredOption.empty, terms: Seq[StructuredTerm]): StructuredTerm =
    operator("phase", option, terms)

  def range[A](field: String, option: StructuredOption = StructuredOption.empty, range: RangeValue[A]): StructuredTerm = {
    val optionString = if (option.query.nonEmpty) {
      option.query + " "
    } else ""
    StructuredQuery(s"(range field=$field $optionString${range.query})")
  }

  def term(option: StructuredOption = StructuredOption.empty, terms: Seq[StructuredTerm]): StructuredTerm =
    operator("term", option, terms)

  private def operator(op: String, option: StructuredOption = StructuredOption.empty, terms: Seq[StructuredTerm]): StructuredTerm = {
    val optionString = if (option.query.nonEmpty) {
      option.query + " "
    } else ""
    terms match {
      case Nil => StructuredQuery.empty
      case _ => {
        StructuredQuery(s"($op $optionString${terms.mkString(" ")})")
      }
    }
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

  def gelt[A](from: Option[A], to: Option[A]): RangeValue[A] = from match {
    case Some(_) => rawGelt(from, to)
    case None => gtlt(None, to)
  }

  def gtle[A](from: Option[A], to: Option[A]): RangeValue[A] = to match {
    case Some(_) => rawGtle(from, to)
    case None => gtlt(None, to)
  }

  def gele[A](from: Option[A], to: Option[A]): RangeValue[A] = (from, to) match {
    case (Some(_), Some(_)) => rawGele(from, to)
    case (None, _) => gtlt(None, to)
    case (_, None) => gelt(from, None)
  }

  /**
    * If from is empty, it returns com.amazonaws.services.cloudsearchdomain.model.SearchException.
    * @param from
    * @param to
    * @tparam A
    * @return
    */
  def rawGelt[A](from: Option[A], to: Option[A]): RangeValue[A] =
    RangeValue[A](containsMin = true, from, to, containsMax = false)


  /**
    * If to is empty, it returns com.amazonaws.services.cloudsearchdomain.model.SearchException.
    * @param from
    * @param to
    * @tparam A
    * @return
    */
  def rawGtle[A](from: Option[A], to: Option[A]): RangeValue[A] =
    RangeValue[A](containsMin = false, from, to, containsMax = true)


  /**
    * If either from or to is empty, it returns com.amazonaws.services.cloudsearchdomain.model.SearchException.
    * @param from
    * @param to
    * @tparam A
    * @return
    */
  def rawGele[A](from: Option[A], to: Option[A]): RangeValue[A] =
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

    implicit def seqStringToSeqStructuredTerm(value: Seq[String]): StructuredTerm =
      and(terms = value.map(v => StructuredQuery(s"'$v'")))

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
