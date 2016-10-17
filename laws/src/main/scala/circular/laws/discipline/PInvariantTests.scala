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
import cats.laws.discipline.catsLawsIsEqToProp
import circular.laws.discipline.arbitrary._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

trait PInvariantTests[F[_]] extends Laws {
  def laws: PInvariantLaws[F]

  def pInvariant[A](implicit AFA: Arbitrary[F[A]], EFA: Eq[F[A]]): RuleSet = new DefaultRuleSet(
    name   = "pInvariant",
    parent = None,
    "pInvariant identity"    -> forAll(laws.pInvariantIdentity[A] _),
    "pInvariant composition" -> forAll(laws.pInvariantComposition[A, A, A] _)
  )
}
