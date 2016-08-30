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

package circular.examples

import argonaut.Json
import argonaut.Argonaut._
import circular._
import circular.argonaut._
import circular.argonaut.syntax.all._
import circular.generic._
import circular.syntax.all._
import org.specs2.Specification

final case class Foo(a: String, b: Boolean)

object Foo {
  val pIso: PIso[(String, Boolean), Foo] = PIso.fromIso((Foo.apply _).tupled, f => (f.a, f.b))
}

sealed abstract class Adt extends Product with Serializable

object Adt {
  final case class Ctor1(a: Boolean, b: Double) extends Adt
  final case class Ctor2(a: Int, b: String)     extends Adt
}

class Example extends Specification {
  def is = s2"""
  Manual example  ${manualExample}
  Derived example ${derivedExample}
  """

  /**
   * {
   *   "foo": "a string",
   *   "bar": {
   *     "baz": true
   *   }
   * }
   */
  def manualExample = {
    import JsonSyntax._

    def syntax[F[_]: JsonSyntax]: F[Foo] =
      Foo.pIso.lift(field("foo", string) <*> field("bar", field("baz", boolean)))

    val str = "a string"
    val bool = true
    val goodJson = Json("foo" := str, "bar" := Json("baz" := bool))
    val badJson = Json("not" := "good")
    val foo = Foo(str, bool)

    (syntax[JsonParser].run(goodJson) must beSome(foo))      and
    (syntax[JsonParser].run(badJson)  must beNone)           and
    (syntax[JsonPrinter].run(foo)     must beSome(goodJson))
  }

  /**
   * Ctor1 = {"boolean": true, "double": 3.14}
   * Ctor2 = {"payload": {"int": 5, "string": "a string"}}
   */
  def derivedExample = {
    import JsonSyntaxConstrained._

    val syntax =
      sum[Adt, Adt.Ctor1].derive.lift(("boolean" ::= boolean) <*> ("double" ::= double))         <+>
      sum[Adt, Adt.Ctor2].derive.lift("payload" ::= (("int" ::= int) <*> ("string" ::= string)))

    val num = 5
    val str = "a string"
    val goodJson = Json("payload" := Json("int" := num, "string" := str))
    val badJson = Json("not" := "good")
    val adt = Adt.Ctor2(num, str)

    (syntax[JsonParser].run(goodJson) must beSome(adt))      and
    (syntax[JsonParser].run(badJson)  must beNone)           and
    (syntax[JsonPrinter].run(adt)     must beSome(goodJson))
  }
}
