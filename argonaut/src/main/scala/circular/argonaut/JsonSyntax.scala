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
package argonaut

import _root_.argonaut.{CodecJson, Json, JsonNumber, JsonObject}
import _root_.argonaut.Argonaut._

/** Type class for defining JSON syntax with partial isomorphisms. */
trait JsonSyntax[F[_]] extends Syntax[F] {
  /**
   * Run the `fa` syntax against some JSON, represented by `fj`.
   * Useful when defining syntax for JSON objects.
   *
   * @param fa Syntax to run against the JSON
   * @param fj The syntax JSON thus far
   */
  def runJson[A](fa: F[A], fj: F[Json]): F[A]

  /** Get the JSON defined by the current syntax. */
  def json: F[Json]
}

object JsonSyntax extends JsonSyntaxFunctions {
  def apply[F[_]](implicit F: JsonSyntax[F]): JsonSyntax[F] = F
}

trait JsonSyntaxFunctions {
  def fromCodec[F[_]: JsonSyntax, A: CodecJson]: F[A] = liftJson(JsonPIso.fromCodec[A])

  def obj[F[_]: JsonSyntax]: F[JsonObject] = liftJson(JsonPIso.obj)

  def key[F[_]: JsonSyntax](k: String): F[Json] = liftJson(JsonPIso.key(k).andThen(JsonPIso.obj))

  def array[F[_]: JsonSyntax]: F[JsonArray] = liftJson(JsonPIso.array)

  def field[F[_]: JsonSyntax, A](k: String, syntax: F[A]): F[A] = JsonSyntax[F].runJson(syntax, key(k))

  def boolean[F[_]: JsonSyntax]: F[Boolean] = liftJson(JsonPIso.boolean)

  def number[F[_]: JsonSyntax]: F[JsonNumber] = liftJson(JsonPIso.number)

  def bigDecimal[F[_]: JsonSyntax]: F[BigDecimal] = liftJson(JsonPIso.bigDecimal)

  def long[F[_]: JsonSyntax]: F[Long] = liftJson(JsonPIso.long)

  def bigInt[F[_]: JsonSyntax]: F[BigInt] = liftJson(JsonPIso.bigInt)

  def byte[F[_]: JsonSyntax]: F[Byte] = liftJson(JsonPIso.byte)

  def double[F[_]: JsonSyntax]: F[Double] = liftJson(JsonPIso.double)

  def float[F[_]: JsonSyntax]: F[Float] = liftJson(JsonPIso.float)

  def int[F[_]: JsonSyntax]: F[Int] = liftJson(JsonPIso.int)

  def short[F[_]: JsonSyntax]: F[Short] = liftJson(JsonPIso.short)

  def string[F[_]: JsonSyntax]: F[String] = liftJson(JsonPIso.string)

  private def liftJson[F[_]: JsonSyntax, A](pIso: PIso[A, Json]): F[A] = pIso.invert.lift(JsonSyntax[F].json)
}
