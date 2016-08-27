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

import argonaut.{DecodeJson, Json}
import cats.Eq
import cats.implicits._

final case class JsonParser[A](run: Json => Option[A]) extends AnyVal

object JsonParser extends JsonParserInstances with JsonParserFunctions

private[circular] sealed abstract class JsonParserInstances {
  implicit val circularJsonSyntaxForJsonParser: JsonSyntax[JsonParser] = new JsonSyntax[JsonParser] {
    def product[A, B](fa: JsonParser[A], fb: JsonParser[B]): JsonParser[(A, B)] = JsonParser { json =>
      val array = json.array.flatMap {
        case Nil    => None
        case h :: t => (fa.run(h), fb.run(Json.jArray(t))).map2((_, _))
      }

      array.orElse((fa.run(json), fb.run(json)).map2((_, _)))
    }

    val json: JsonParser[Json] = JsonParser(Some(_))

    def runJson[A](fa: JsonParser[A], fj: JsonParser[Json]): JsonParser[A] = JsonParser { json =>
      for {
        j <- fj.run(json)
        a <- fa.run(j)
      } yield a
    }

    def empty[A]: JsonParser[A] = JsonParser(Function.const(None))

    def pmap[A, B](fa: JsonParser[A])(f: Prism[A, B]): JsonParser[B] = JsonParser { json =>
      for {
        a <- fa.run(json)
        b <- f.to(a)
      } yield b
    }

    def combineK[A](x: JsonParser[A], y: JsonParser[A]): JsonParser[A] =
      JsonParser(json => x.run(json).orElse(y.run(json)))

    def pure[A: Eq](a: A): JsonParser[A] = JsonParser(Function.const(Some(a)))
  }
}

trait JsonParserFunctions {
  def derive[A: DecodeJson]: JsonParser[A] = JsonParser(json => DecodeJson.of[A].decodeJson(json).toOption)

  val id: JsonParser[Json] = JsonParser(Some(_))
}
