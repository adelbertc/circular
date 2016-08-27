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

final case class Prism[A, B](to: A => Option[B], from: B => A) {
  def lift[F[_]: PrismFunctor](fa: F[A]): F[B] =
    PrismFunctor[F].pmap(fa)(this)

  def compose[C](other: Prism[C, A]): Prism[C, B] = Prism(c => other.to(c).flatMap(to), from.andThen(other.from))

  def andThen[C](other: Prism[B, C]): Prism[A, C] = other.compose(this)

  def isoMap[C](f: B => C, g: C => B): Prism[A, C] = Prism(a => to(a).map(f), g.andThen(from))
}

object Prism extends PrismInstances with PrismFunctions

private[circular] sealed abstract class PrismInstances {
  implicit val circularCategoryForPrism: Category[Prism] = new Category[Prism] {
    def compose[A, B, C](f: Prism[B, C], g: Prism[A, B]): Prism[A, C] = f.compose(g)

    def id[A]: Prism[A, A] = Prism(Some(_), identity)
  }
}

trait PrismFunctions {
  def id[A]: Prism[A, A] = Prism(Some(_), identity)

  def fromIso[A, B](to: A => B, from: B => A): Prism[A, B] =
    Prism(a => Some(to(a)), from)
}
