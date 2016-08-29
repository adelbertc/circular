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

import _root_.argonaut.{CodecJson, EncodeJson, Json, JsonNumber, JsonObject}
import _root_.argonaut.Argonaut.JsonArray

object JsonPIso {
  def fromCodec[A](implicit A: CodecJson[A]): PIso[A, Json] =
    PIso(a => Some(A.encode(a)), json => A.decodeJson(json).toOption)

  val obj: PIso[JsonObject, Json] = PIso.fromPrismR(Json.jObject, _.obj)

  def key(k: String): PIso[Json, JsonObject] = PIso.fromPrismR(JsonObject.single(k, _), _(k))

  val array: PIso[JsonArray, Json] = PIso.fromPrismR(Json.jArray, _.array)

  def field[A](k: String, pIso: PIso[A, Json]): PIso[A, Json] = pIso.andThen(key(k)).andThen(obj)

  val boolean: PIso[Boolean, Json] = PIso.fromPrismR(Json.jBool, _.bool)

  val number: PIso[JsonNumber, Json] = PIso.fromPrismR(Json.jNumber, _.number)

  val bigDecimal: PIso[BigDecimal, Json] =
    PIso[BigDecimal, JsonNumber](wrap[BigDecimal], jnum => Some(jnum.toBigDecimal)).andThen(number)

  val long: PIso[Long, Json] = PIso[Long, JsonNumber](wrap[Long], _.toLong).andThen(number)

  val bigInt: PIso[BigInt, Json] = PIso[BigInt, JsonNumber](wrap[BigInt], _.toBigInt).andThen(number)

  val byte: PIso[Byte, Json] = PIso[Byte, JsonNumber](wrap[Byte], _.toByte).andThen(number)

  val double: PIso[Double, Json] = PIso[Double, JsonNumber](wrap[Double], _.toDouble).andThen(number)

  val float: PIso[Float, Json] = PIso[Float, JsonNumber](wrap[Float], _.toFloat).andThen(number)

  val int: PIso[Int, Json] = PIso[Int, JsonNumber](wrap[Int], _.toInt).andThen(number)

  val short: PIso[Short, Json] = PIso[Short, JsonNumber](wrap[Short], _.toShort).andThen(number)

  val string: PIso[String, Json] = PIso.fromPrismR(Json.jString, _.string)

  private def wrap[A](a: A)(implicit A: EncodeJson[A]): Option[JsonNumber] =
    A.encode(a).number
}
