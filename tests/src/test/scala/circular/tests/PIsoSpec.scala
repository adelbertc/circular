/*
 * Copyright 2016 Adelbert Chang
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package circular
package tests

import cats.implicits._
import cats.laws.discipline.{CategoryTests, SplitTests}
import circular.laws.discipline.PIsoTests
import circular.laws.discipline.arbitrary._
import circular.laws.discipline.eq._
import org.specs2.{ScalaCheck, Specification}
import org.typelevel.discipline.specs2.Discipline

class PIsoSpec extends Specification with ScalaCheck with Discipline { def is =
  s2"""
  ${checkAll("PIso",      CategoryTests[PIso].category[Int, Int, Int, Int])}
  ${checkAll("PIso",      SplitTests[PIso].split[Int, Int, Int, Int, Int, Int])}

  ${checkAll("first",     PIsoTests.pIso(PIso.cons[Int].first[Char]))}
  ${checkAll("second",    PIsoTests.pIso(PIso.cons[Int].second[Char]))}
  ${checkAll("iterate",   PIsoTests.pIso(PIso.subset[Int](Function.const(false)).iterate))}
  ${checkAll("foldLeft",  PIsoTests.pIso(PIso.foldLeft(PIso.commute[List[Int], Int].andThen(PIso.cons[Int]))))}

  ${checkAll("id",        PIsoTests.pIso(PIso.id[Int]))}
  ${checkAll("fromPrism", PIsoTests.pIso(PIso.fromPrism[List[Int], Int](_.headOption, List(_))))}
  ${checkAll("fromPrism", PIsoTests.pIso(PIso.fromPrismR[Int, List[Int]](List(_), _.headOption)))}
  ${checkAll("nil",       PIsoTests.pIso(PIso.nil[Int]))}
  ${checkAll("cons",      PIsoTests.pIso(PIso.cons[Int]))}
  ${checkAll("vnil",      PIsoTests.pIso(PIso.vnil[Int]))}
  ${checkAll("vcons",     PIsoTests.pIso(PIso.vcons[Int]))}
  ${checkAll("none",      PIsoTests.pIso(PIso.none[Int]))}
  ${checkAll("some",      PIsoTests.pIso(PIso.some[Int]))}
  ${checkAll("left",      PIsoTests.pIso(PIso.left[Int, String]))}
  ${checkAll("right",     PIsoTests.pIso(PIso.right[Int, String]))}
  ${checkAll("associate", PIsoTests.pIso(PIso.associate[Int, String, Double]))}
  ${checkAll("commute",   PIsoTests.pIso(PIso.commute[Int, String]))}
  ${checkAll("unit",      PIsoTests.pIso(PIso.unit[Int]))}

  ${checkAll("element",   PIsoTests.pIso(PIso.element(17)))}
  ${checkAll("subset",    PIsoTests.pIso(PIso.subset[Int](_ % 2 == 0)))}
  """
}
