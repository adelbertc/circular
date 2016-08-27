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

import argonaut.{Json, JsonNumber, JsonObject}
import argonaut.Argonaut._

trait JsonSyntax[F[_]] extends Syntax[F] {
  def runJson[A](fa: F[A], fj: F[Json]): F[A]

  def json: F[Json]
}

object JsonSyntax extends JsonSyntaxFunctions {
  def apply[F[_]](implicit F: JsonSyntax[F]): JsonSyntax[F] = F
}

trait JsonSyntaxFunctions {
  val jsonFieldPrism: Prism[Json, JsonObject] = Prism(_.obj, Json.jObject)

  def jsonField[F[_]: JsonSyntax, A](key: String, syntax: F[A]): F[A] = {
    val keyPrism: Prism[Json, Json] = Prism(_.obj.flatMap(_(key)), json => Json(key := json))
    JsonSyntax[F].runJson(syntax, keyPrism.lift(JsonSyntax[F].json))
  }

  val jsonBooleanPrism: Prism[Json, Boolean] = Prism(_.bool, Json.jBool)

  def jsonBoolean[F[_]: JsonSyntax]: F[Boolean] = liftJson(jsonBooleanPrism)

  val jsonNumberPrism: Prism[Json, JsonNumber] = Prism(_.number, Json.jNumber)

  def jsonNumber[F[_]: JsonSyntax]: F[JsonNumber] = liftJson(jsonNumberPrism)

  val jsonStringPrism: Prism[Json, String] = Prism(_.string, Json.jString)

  def jsonString[F[_]: JsonSyntax]: F[String] = liftJson(jsonStringPrism)

  private def liftJson[F[_]: JsonSyntax, A](prism: Prism[Json, A]): F[A] = prism.lift(JsonSyntax[F].json)
}
