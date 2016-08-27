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

import cats.arrow.Category
import cats.implicits._

final case class PIso[A, B](to: A => Option[B], from: B => Option[A]) {
  def lift[F[_]: PIsoFunctor](fa: F[A]): F[B] =
    PIsoFunctor[F].pimap(fa)(this)

  def invert: PIso[B, A] = PIso(from, to)

  def compose[C](other: PIso[C, A]): PIso[C, B] = PIso(c => other.to(c).flatMap(to), b => from(b).flatMap(other.from))

  def andThen[C](other: PIso[B, C]): PIso[A, C] = other.compose(this)
}

object PIso extends PIsoInstances with PIsoFunctions

private[circular] sealed abstract class PIsoInstances {
  implicit val circularCategoryForPIso: Category[PIso] = new Category[PIso] {
    def compose[A, B, C](f: PIso[B, C], g: PIso[A, B]): PIso[A, C] = f.compose(g)

    def id[A]: PIso[A, A] = PIso(Some(_), Some(_))
  }
}

trait PIsoFunctions {
  def id[A]: PIso[A, A] = PIso(Some(_), Some(_))

  def fromIso[A, B](to: A => B, from: B => A): PIso[A, B] =
    PIso(a => Some(to(a)), b => Some(from(b)))

  def fromPrism[A, B](to: A => Option[B], from: B => A): PIso[A, B] = PIso(to, b => Some(from(b)))
}
