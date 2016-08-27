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
  def fromCodec[A](implicit A: CodecJson[A]): PIso[Json, A] =
    PIso(json => A.decodeJson(json).toOption, a => Some(A.encode(a)))

  val obj: PIso[Json, JsonObject] = PIso.fromPrism(_.obj, Json.jObject)

  def key(k: String): PIso[JsonObject, Json] = PIso.fromPrism(_(k), JsonObject.single(k, _))

  val array: PIso[Json, JsonArray] = PIso.fromPrism(_.array, Json.jArray)

  def field[A](k: String, pIso: PIso[Json, A]): PIso[Json, A] =
    obj.andThen(key(k)).andThen(pIso)

  val boolean: PIso[Json, Boolean] = PIso.fromPrism(_.bool, Json.jBool)

  val number: PIso[Json, JsonNumber] = PIso.fromPrism(_.number, Json.jNumber)

  val bigDecimal: PIso[Json, BigDecimal] = number.andThen(PIso(jnum => Some(jnum.toBigDecimal), wrap[BigDecimal]))

  val long: PIso[Json, Long] = number.andThen(PIso(_.toLong, wrap[Long]))

  val bigInt: PIso[Json, BigInt] = number.andThen(PIso(_.toBigInt, wrap[BigInt]))

  val byte: PIso[Json, Byte] = number.andThen(PIso(_.toByte, wrap[Byte]))

  val double: PIso[Json, Double] = number.andThen(PIso(_.toDouble, wrap[Double]))

  val float: PIso[Json, Float] = number.andThen(PIso(_.toFloat, wrap[Float]))

  val int: PIso[Json, Int] = number.andThen(PIso(_.toInt, wrap[Int]))

  val short: PIso[Json, Short] = number.andThen(PIso(_.toShort, wrap[Short]))

  val string: PIso[Json, String] = PIso.fromPrism(_.string, Json.jString)

  private def wrap[A](a: A)(implicit A: EncodeJson[A]): Option[JsonNumber] =
    A.encode(a).number
}
