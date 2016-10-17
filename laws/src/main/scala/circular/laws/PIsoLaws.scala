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

import cats.laws.{IsEq, IsEqArrow}

object PIsoLaws {
  def to[A, B](p: PIso[A, B], a: A): IsEq[Option[B]] =
    p.to(a).flatMap(p.from).flatMap(p.to) <-> p.to(a)

  def from[A, B](p: PIso[A, B], b: B): IsEq[Option[A]] =
    p.from(b).flatMap(p.to).flatMap(p.from) <-> p.from(b)
}
