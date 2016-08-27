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
import _root_.argonaut.Argonaut.JsonArray
import cats.Eq

trait JsonSyntaxConstrained[A] { fa =>
  def apply[F[_]: JsonSyntax]: F[A]
}

object JsonSyntaxConstrained extends JsonSyntaxConstrainedInstances with JsonSyntaxConstrainedFunctions

private[circular] sealed abstract class JsonSyntaxConstrainedInstances {
  implicit val circularArgonautPIsoFunctorForJsonSyntax: JsonSyntax[JsonSyntaxConstrained] =
    new JsonSyntax[JsonSyntaxConstrained] {
      def product[A, B](fa: JsonSyntaxConstrained[A], fb: JsonSyntaxConstrained[B]): JsonSyntaxConstrained[(A, B)] =
        new JsonSyntaxConstrained[(A, B)] {
          def apply[F[_]: JsonSyntax]: F[(A, B)] =
            JsonSyntax[F].product(fa[F], fb[F])
        }

      val json: JsonSyntaxConstrained[Json] = new JsonSyntaxConstrained[Json] {
        def apply[F[_]: JsonSyntax]: F[Json] = JsonSyntax[F].json
      }

      def runJson[A](fa: JsonSyntaxConstrained[A], fj: JsonSyntaxConstrained[Json]): JsonSyntaxConstrained[A] =
        new JsonSyntaxConstrained[A] {
          def apply[F[_]: JsonSyntax]: F[A] = JsonSyntax[F].runJson(fa[F], fj[F])
        }

      def empty[A]: JsonSyntaxConstrained[A] =
        new JsonSyntaxConstrained[A] {
          def apply[F[_]: JsonSyntax]: F[A] = JsonSyntax[F].empty[A]
        }

      def pimap[A, B](fa: JsonSyntaxConstrained[A])(f: PIso[A, B]): JsonSyntaxConstrained[B] =
        new JsonSyntaxConstrained[B] {
          def apply[F[_]: JsonSyntax]: F[B] = JsonSyntax[F].pimap(fa[F])(f)
        }

      def combineK[A](x: JsonSyntaxConstrained[A], y: JsonSyntaxConstrained[A]): JsonSyntaxConstrained[A] =
        new JsonSyntaxConstrained[A] {
          def apply[F[_]: JsonSyntax]: F[A] = JsonSyntax[F].combineK(x[F], y[F])
        }

      def pure[A: Eq](a: A): JsonSyntaxConstrained[A] =
        new JsonSyntaxConstrained[A] {
          def apply[F[_]: JsonSyntax]: F[A] = JsonSyntax[F].pure(a)
        }
  }
}

trait JsonSyntaxConstrainedFunctions {
  def fromCodec[A: CodecJson]: JsonSyntaxConstrained[A] = new JsonSyntaxConstrained[A] {
    def apply[F[_]: JsonSyntax]: F[A] = JsonSyntax.fromCodec
  }

  val obj: JsonSyntaxConstrained[JsonObject] = new JsonSyntaxConstrained[JsonObject] {
    def apply[F[_]: JsonSyntax]: F[JsonObject] = JsonSyntax.obj
  }

  def key(k: String): JsonSyntaxConstrained[Json] = new JsonSyntaxConstrained[Json] {
    def apply[F[_]: JsonSyntax]: F[Json] = JsonSyntax.key(k)
  }

  val array: JsonSyntaxConstrained[JsonArray] = new JsonSyntaxConstrained[JsonArray] {
    def apply[F[_]: JsonSyntax]: F[JsonArray] = JsonSyntax.array
  }

  def field[A](k: String, pIso: JsonSyntaxConstrained[A]): JsonSyntaxConstrained[A] = new JsonSyntaxConstrained[A] {
    def apply[F[_]: JsonSyntax]: F[A] = JsonSyntax.field(k, pIso[F])
  }

  val boolean: JsonSyntaxConstrained[Boolean] = new JsonSyntaxConstrained[Boolean] {
    def apply[F[_]: JsonSyntax]: F[Boolean] = JsonSyntax.boolean
  }

  val number: JsonSyntaxConstrained[JsonNumber] = new JsonSyntaxConstrained[JsonNumber] {
    def apply[F[_]: JsonSyntax]: F[JsonNumber] = JsonSyntax.number
  }

  val bigDecimal: JsonSyntaxConstrained[BigDecimal] = new JsonSyntaxConstrained[BigDecimal] {
    def apply[F[_]: JsonSyntax]: F[BigDecimal] = JsonSyntax.bigDecimal
  }

  val long: JsonSyntaxConstrained[Long] = new JsonSyntaxConstrained[Long] {
    def apply[F[_]: JsonSyntax]: F[Long] = JsonSyntax.long
  }

  val bigInt: JsonSyntaxConstrained[BigInt] = new JsonSyntaxConstrained[BigInt] {
    def apply[F[_]: JsonSyntax]: F[BigInt] = JsonSyntax.bigInt
  }

  val byte: JsonSyntaxConstrained[Byte] = new JsonSyntaxConstrained[Byte] {
    def apply[F[_]: JsonSyntax]: F[Byte] = JsonSyntax.byte
  }

  val double: JsonSyntaxConstrained[Double] = new JsonSyntaxConstrained[Double] {
    def apply[F[_]: JsonSyntax]: F[Double] = JsonSyntax.double
  }

  val float: JsonSyntaxConstrained[Float] = new JsonSyntaxConstrained[Float] {
    def apply[F[_]: JsonSyntax]: F[Float] = JsonSyntax.float
  }

  val int: JsonSyntaxConstrained[Int] = new JsonSyntaxConstrained[Int] {
    def apply[F[_]: JsonSyntax]: F[Int] = JsonSyntax.int
  }

  val short: JsonSyntaxConstrained[Short] = new JsonSyntaxConstrained[Short] {
    def apply[F[_]: JsonSyntax]: F[Short] = JsonSyntax.short
  }

  val string: JsonSyntaxConstrained[String] = new JsonSyntaxConstrained[String] {
    def apply[F[_]: JsonSyntax]: F[String] = JsonSyntax.string
  }
}
