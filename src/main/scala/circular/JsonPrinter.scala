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

import argonaut.{EncodeJson, Json, JsonObject}
import cats.Eq
import cats.implicits._

final case class JsonPrinter[A](run: A => Option[Json]) extends AnyVal

object JsonPrinter extends JsonPrinterInstances with JsonPrinterFunctions

private[circular] sealed abstract class JsonPrinterInstances {
  implicit val circularJsonSyntaxForJsonPrinter: JsonSyntax[JsonPrinter] = new JsonSyntax[JsonPrinter] {
    def product[A, B](fa: JsonPrinter[A], fb: JsonPrinter[B]): JsonPrinter[(A, B)] = {
      def merge(x: Json, y: Json): Option[Json] = {
        val objects = (x.obj, y.obj).map2 { (am, bm) =>
          Json.jObject(JsonObject.fromTraversableOnce(am.toMap ++ bm.toMap))
        }

        val cons = y.array.map(x :: _).map(Json.jArray)

        val rightNull = if (y.isNull) Some(x) else None

        val leftNull = if (x.isNull) Some(y) else None

        objects.orElse(cons).orElse(rightNull).orElse(leftNull)
      }

      JsonPrinter { case (a, b) =>
        for {
          aa <- fa.run(a)
          bb <- fb.run(b)
          p  <- merge(aa, bb)
        } yield p
      }
    }

    val json: JsonPrinter[Json] = JsonPrinter(Some(_))

    def runJson[A](fa: JsonPrinter[A], fj: JsonPrinter[Json]): JsonPrinter[A] =
      JsonPrinter(a => fa.run(a).flatMap(fj.run))

    def empty[A]: JsonPrinter[A] = JsonPrinter(Function.const(None))

    def pmap[A, B](fa: JsonPrinter[A])(f: Prism[A, B]): JsonPrinter[B] =
      JsonPrinter(f.from.andThen(fa.run))

    def combineK[A](x: JsonPrinter[A], y: JsonPrinter[A]): JsonPrinter[A] =
      JsonPrinter(a => x.run(a).orElse(y.run(a)))

    def pure[A: Eq](a: A): JsonPrinter[A] = JsonPrinter(aa => if (Eq[A].eqv(a, aa)) Some(Json.jNull) else None)
  }
}

trait JsonPrinterFunctions {
  def derive[A: EncodeJson]: JsonPrinter[A] = JsonPrinter(a => Some(EncodeJson.of[A].encode(a)))

  val id: JsonPrinter[Json] = JsonPrinter(Some(_))
}
