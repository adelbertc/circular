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

import _root_.argonaut.{CursorHistory, DecodeJson, DecodeResult, Json}
import _root_.argonaut.Argonaut.JsonArray
import cats.Eq
import cats.implicits._

final case class JsonParser[A](run: Json => DecodeResult[A]) extends AnyVal

object JsonParser extends JsonParserInstances with JsonParserFunctions

private[circular] sealed abstract class JsonParserInstances {
  implicit val circularJsonSyntaxForJsonParser: JsonSyntax[JsonParser] = new JsonSyntax[JsonParser] {
    private def productDecodeResult[A, B](da: DecodeResult[A], db: DecodeResult[B]): DecodeResult[(A, B)] = for {
      a <- da
      b <- db
    } yield (a, b)

    def product[A, B](fa: JsonParser[A], fb: JsonParser[B]): JsonParser[(A, B)] = JsonParser { json =>
      val arrayCursor = json.hcursor.downArray
      val nelOpt = (arrayCursor.focus, arrayCursor.rights).map2((_, _))

      val nelDecode =
        nelOpt.fold(
          DecodeResult.fail[(Json, JsonArray)](s"JsonSyntax[JsonParser].product - empty array", arrayCursor.history)
        )(DecodeResult.ok)

      val nelResult = for {
        nel <- nelDecode
        (h, t) = nel
        res <- productDecodeResult(fa.run(h), fb.run(Json.jArray(t)))
      } yield res

      nelResult ||| productDecodeResult(fa.run(json), fb.run(json))
    }

    val json: JsonParser[Json] = JsonParser(DecodeResult.ok)

    def runJson[A](fa: JsonParser[A], fj: JsonParser[Json]): JsonParser[A] = JsonParser { json =>
      for {
        j <- fj.run(json)
        a <- fa.run(j)
      } yield a
    }

    def empty[A]: JsonParser[A] =
      JsonParser(Function.const(DecodeResult.fail("JsonSyntax[JsonParser].empty", CursorHistory.empty)))

    def pimap[A, B](fa: JsonParser[A])(f: PIso[A, B]): JsonParser[B] = JsonParser { json =>
      for {
        a <- fa.run(json)
        b <- f.to(a).fold(DecodeResult.fail[B](s"JsonSyntax[JsonProduct].pimap", CursorHistory.empty))(DecodeResult.ok)
      } yield b
    }

    def combineK[A](x: JsonParser[A], y: JsonParser[A]): JsonParser[A] =
      JsonParser(json => x.run(json) ||| y.run(json))

    def pure[A: Eq](a: A): JsonParser[A] = JsonParser(Function.const(DecodeResult.ok(a)))
  }
}

trait JsonParserFunctions {
  def derive[A: DecodeJson]: JsonParser[A] = JsonParser(json => DecodeJson.of[A].decodeJson(json))

  val id: JsonParser[Json] = JsonParser(DecodeResult.ok)
}
