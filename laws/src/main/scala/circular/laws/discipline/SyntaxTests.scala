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
import cats.laws.discipline.{MonoidKTests, CartesianTests, catsLawsIsEqToProp}
import cats.laws.discipline.CartesianTests.Isomorphisms
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait SyntaxTests[F[_]] extends CartesianTests[F] with MonoidKTests[F] with PInvariantTests[F] {
  def laws: SyntaxLaws[F]

  def syntax[A, B, C](implicit
    AA   : Arbitrary[A],
    AFA  : Arbitrary[F[A]],
    AB   : Arbitrary[B],
    AFB  : Arbitrary[F[B]],
    AC   : Arbitrary[C],
    AFC  : Arbitrary[F[C]],
    EB   : Eq[B],
    EFA  : Eq[F[A]],
    EFABC: Eq[F[(A, B, C)]],
    IF   : Isomorphisms[F]
  ): RuleSet = new RuleSet {
    val f = PIso.fromIso[(A, (B, C)), (A, B, C)]( { case (a, (b, c)) => (a, b, c) }, { case (a, b, c) => (a, (b, c)) })
    implicit val fabcAssoc: Eq[F[(A, (B, C))]] = EFABC.on(fabc => laws.F.pimap(fabc)(f))
    val name    = "syntax"
    val parents = List(cartesian[A, B, C], monoidK[A], pInvariant[A])
    val bases   = List.empty
    val props   = List(
      "syntax left identity"  -> forAll(laws.syntaxLeftIdentity[A, B] _),
      "syntax right identity" -> forAll(laws.syntaxRightIdentity[A, B] _),
      "syntax associativity"  -> forAll(laws.syntaxAssociativity[A, B, C] _)
    )
  }
}

object SyntaxTests {
  def apply[F[_]: Syntax]: SyntaxTests[F] =
    new SyntaxTests[F] { def laws: SyntaxLaws[F] = SyntaxLaws[F] }
}
