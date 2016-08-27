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

package circular.syntax

import cats.Cartesian

trait CartesianSyntax {
  implicit def circularSyntaxCartesian[F[_], A](fa: F[A]): CartesianOps[F, A] =
    new CartesianOps(fa)
}

final class CartesianOps[F[_], A](val fa: F[A]) extends AnyVal {
  def <*>[B](fb: F[B])(implicit F: Cartesian[F]): F[(A, B)] = F.product(fa, fb)
}
