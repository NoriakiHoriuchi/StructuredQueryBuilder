package com.noriakihoriuchi.sqb

import StructuredQueryBuilder.Implicits._
import StructuredQueryBuilder.{not => sqNot, _}
import org.joda.time.DateTime
import org.scalatest._

class StructuredQueryBuilderSpec extends FlatSpec with Matchers {
  "StructuredQueryBuilder" should "pass tests" in {
    matchall.build should ===("matchall")

    and(StructuredOption.empty, Seq("keyString" -> "valueString")).build should ===("(and keyString:'valueString')")

    and(StructuredOption.empty, Seq("keyString" -> new DateTime(0))).build should ===("(and keyString:'1970-01-01T00:00:00.000Z')")

    and(StructuredOption.empty, Seq("keyString" -> 3)).build should ===("(and keyString:3)")

    and(StructuredOption.empty, Seq("keyString" -> 3.0)).build should ===("(and keyString:3.0)")

    and(StructuredOption.empty, Seq("keyString" -> BigDecimal(3.0))).build should ===("(and keyString:3.0)")

    // Set alias of "not" as "sqNot"; Due to the limitation of ScalaTest, the "not" function of StructuredQueryBuilder cannot be used in the class extending scalatest.Matchers trait.
    and(StructuredOption.empty, Seq("keyString" -> "valueString", sqNot(StructuredOption.empty, Seq("keyString2" -> "valueString2")))).build should ===("(and keyString:'valueString' (not keyString2:'valueString2'))")

    and(StructuredOption.empty, Seq("keyString" -> "valueString", "keyString2" -> "valueString2")).build should ===("(and keyString:'valueString' keyString2:'valueString2')")

    and(StructuredOption.empty, Seq("keyString" -> "valueString", "keyString2" -> "valueString2", or(StructuredOption.empty, Seq("keyString3" -> "valueString3")))).build should ===("(and keyString:'valueString' keyString2:'valueString2' (or keyString3:'valueString3'))")

    and(StructuredOption.empty, Seq("keyString" -> "valueString", "keyString2" -> "valueString2", or(field("keyString3"), Seq("valueString3")))).build should ===("(and keyString:'valueString' keyString2:'valueString2' (or field=keyString3 'valueString3'))")

    and(boost(1), Seq("keyString" -> "valueString", "keyString2" -> "valueString2", or(field("keyString3"), Seq("valueString3")))).build should ===("(and boost=1 keyString:'valueString' keyString2:'valueString2' (or field=keyString3 'valueString3'))")

    and(boost(1), Seq(prefix(StructuredOption.empty, Seq("keyString" -> "valueString")), "keyString2" -> "valueString2", or(field("keyString3"), Seq("valueString3")))).build should ===("(and boost=1 (prefix keyString:'valueString') keyString2:'valueString2' (or field=keyString3 'valueString3'))")

    and(boost(1), Seq(phase(StructuredOption.empty, Seq("keyString" -> "valueString")), "keyString2" -> "valueString2", or(field("keyString3"), Seq("valueString3")))).build should ===("(and boost=1 (phase keyString:'valueString') keyString2:'valueString2' (or field=keyString3 'valueString3'))")

    and(field("keyString"), Seq("valueString")).build should ===("(and field=keyString 'valueString')")

    term(StructuredOption.empty, Seq("keyString" -> "valueString")).build should ===("(term keyString:'valueString')")

    near(field("keyString") :+ distance(10) :+ boost(1), Seq("valueString", "valueString2")).build should ===("(near field=keyString distance=10 boost=1 'valueString' 'valueString2')")

    and(field("keyInt"), Seq(1)).build should ===("(and field=keyInt 1)")

    and(field("keyDouble"), Seq(1.0)).build should ===("(and field=keyDouble 1.0)")

    and(field("keyBigDecimal"), Seq(BigDecimal(1.0))).build should ===("(and field=keyBigDecimal 1.0)")

    and(field("keyJodaDateTime"), Seq(new DateTime(0))).build should ===("(and field=keyJodaDateTime '1970-01-01T00:00:00.000Z')")

    range("id", StructuredOption.empty, gtlt(Some(1), Some(3))).build should ===("(range field=id {1,3})")

    range("id", StructuredOption.empty, gtle(None, Some(3))).build should ===("(range field=id {,3])")

    range("createdAt", StructuredOption.empty, gelt(Some(new DateTime(0)), None)).build should ===("(range field=createdAt ['1970-01-01T00:00:00.000Z',})")

    range("createdAt", StructuredOption.empty, gele(Some(new DateTime(0)), Some(new DateTime(0)))).build should ===("(range field=createdAt ['1970-01-01T00:00:00.000Z','1970-01-01T00:00:00.000Z'])")
  }
}
