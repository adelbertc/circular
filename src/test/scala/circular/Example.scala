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

import argonaut.Json
import argonaut.Argonaut._
import cats.implicits._
import circular.JsonSyntax._
import org.specs2.Specification

class Example extends Specification {
  def is = s2"""
  Example ${example}
  """

  /**
   * {
   *   "foo": "a string",
   *   "bar": {
   *     "baz": true
   *   }
   * }
   */
  def quuxSyntax[F[_]: JsonSyntax]: F[Quux] = {
    val s = jsonField("foo", jsonString).product(
            jsonField("bar", jsonField(
                             "baz", jsonBoolean)))
    Quux.quuxPrism.lift(s)
  }

  def example = {
    val str = "a string"
    val bool = true
    val goodJson = Json("foo" := str, "bar" := Json("baz" := bool))
    val badJson = Json("not" := "good")
    val quux = Quux(str, bool)

    (quuxSyntax[JsonParser].run(goodJson) must beSome(quux))     and
    (quuxSyntax[JsonParser].run(badJson)  must beNone)           and
    (quuxSyntax[JsonPrinter].run(quux)    must beSome(goodJson))
  }
}

final case class Quux(foo: String, baz: Boolean)

object Quux {
  // This could be auto-derived (Shapeless ?)
  val quuxPrism: Prism[(String, Boolean), Quux] = Prism.fromIso(
    { case (s, b) => Quux(s, b) },
    q => (q.foo, q.baz)
  )
}
