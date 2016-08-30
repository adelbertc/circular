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

import cats.{Cartesian, Eq, MonoidK}

/** Type class for defining syntax with partial isomorphisms. */
trait Syntax[F[_]] extends MonoidK[F] with Cartesian[F] with PInvariant[F] {
  def pure[A: Eq](a: A): F[A]
}

object Syntax {
  def apply[F[_]](implicit F: Syntax[F]): Syntax[F] = F
}
