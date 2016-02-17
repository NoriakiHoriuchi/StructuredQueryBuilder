package com.noriakihoriuchi.sqb

import StructuredQueryBuilder.Implicits._
import StructuredQueryBuilder.{not => sqNot, _}
import org.joda.time.DateTime
import org.scalatest._

class StructuredQueryBuilderSpec extends FlatSpec with Matchers {
  "StructuredQueryBuilder" should "pass tests" in {
    matchall.build should ===("matchall")

    and(Nil, Seq("keyString" -> "valueString")).build should ===("(and keyString:'valueString')")

    and(Nil, Seq("keyString" -> new DateTime(0))).build should ===("(and keyString:'1970-01-01T00:00:00.000Z')")

    and(Nil, Seq("keyString" -> 3)).build should ===("(and keyString:3)")

    and(Nil, Seq("keyString" -> 3.0)).build should ===("(and keyString:3.0)")

    and(Nil, Seq("keyString" -> BigDecimal(3.0))).build should ===("(and keyString:3.0)")

    // Set alias of "not" as "sqNot"; Due to the limitation of ScalaTest, the "not" function of StructuredQueryBuilder cannot be used in the class extending scalatest.Matchers trait.
    and(Nil, Seq("keyString" -> "valueString", sqNot(Nil, Seq("keyString2" -> "valueString2")))).build should ===("(and keyString:'valueString' (not keyString2:'valueString2'))")

    and(Nil, Seq("keyString" -> "valueString", "keyString2" -> "valueString2")).build should ===("(and keyString:'valueString' keyString2:'valueString2')")

    and(Nil, Seq("keyString" -> "valueString", "keyString2" -> "valueString2", or(Nil, Seq("keyString3" -> "valueString3")))).build should ===("(and keyString:'valueString' keyString2:'valueString2' (or keyString3:'valueString3'))")

    and(Nil, Seq("keyString" -> "valueString", "keyString2" -> "valueString2", or(Seq(field("keyString3")), Seq("valueString3")))).build should ===("(and keyString:'valueString' keyString2:'valueString2' (or field=keyString3 'valueString3'))")

    and(Seq(boost(1)), Seq("keyString" -> "valueString", "keyString2" -> "valueString2", or(Seq(field("keyString3")), Seq("valueString3")))).build should ===("(and boost=1 keyString:'valueString' keyString2:'valueString2' (or field=keyString3 'valueString3'))")

    and(Seq(boost(1)), Seq(prefix(Nil, Seq("keyString" -> "valueString")), "keyString2" -> "valueString2", or(Seq(field("keyString3")), Seq("valueString3")))).build should ===("(and boost=1 (prefix keyString:'valueString') keyString2:'valueString2' (or field=keyString3 'valueString3'))")

    and(Seq(boost(1)), Seq(phase(Nil, Seq("keyString" -> "valueString")), "keyString2" -> "valueString2", or(Seq(field("keyString3")), Seq("valueString3")))).build should ===("(and boost=1 (phase keyString:'valueString') keyString2:'valueString2' (or field=keyString3 'valueString3'))")

    and(Seq(field("keyString")), Seq("valueString")).build should ===("(and field=keyString 'valueString')")

    term(Nil, Seq("keyString" -> "valueString")).build should ===("(term keyString:'valueString')")

    near(Seq(field("keyString"), distance(10), boost(1)), Seq("valueString", "valueString2")).build should ===("(near field=keyString distance=10 boost=1 'valueString' 'valueString2')")

    and(Seq(field("keyInt")), Seq(1)).build should ===("(and field=keyInt 1)")

    and(Seq(field("keyDouble")), Seq(1.0)).build should ===("(and field=keyDouble 1.0)")

    and(Seq(field("keyBigDecimal")), Seq(BigDecimal(1.0))).build should ===("(and field=keyBigDecimal 1.0)")

    and(Seq(field("keyJodaDateTime")), Seq(new DateTime(0))).build should ===("(and field=keyJodaDateTime '1970-01-01T00:00:00.000Z')")

    range("id", Nil, gtlt(Some(1), Some(3))).build should ===("(range field=id {1,3})")

    range("id", Nil, gtle(None, Some(3))).build should ===("(range field=id {,3])")

    range("createdAt", Nil, gelt(Some(new DateTime(0)), None)).build should ===("(range field=createdAt ['1970-01-01T00:00:00.000Z',})")

    range("createdAt", Nil, gele(Some(new DateTime(0)), Some(new DateTime(0)))).build should ===("(range field=createdAt ['1970-01-01T00:00:00.000Z','1970-01-01T00:00:00.000Z'])")
  }
}
