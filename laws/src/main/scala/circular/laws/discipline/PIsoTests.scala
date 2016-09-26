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
package laws
package discipline

import cats.Eq
import cats.implicits._
import cats.laws.discipline.catsLawsIsEqToProp
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

object PIsoTests extends Laws {
  def pIso[A, B](p: PIso[A, B])(implicit
    AA: Arbitrary[A],
    EA: Eq[A],
    AB: Arbitrary[B],
    EB: Eq[B]
  ): RuleSet = new DefaultRuleSet(
    name   = "pIso",
    parent = None,
    "to . from . to = to"     -> forAll(PIsoLaws.to(p, _: A)),
    "from . to . from = from" -> forAll(PIsoLaws.from(p, _: B))
  )
}
