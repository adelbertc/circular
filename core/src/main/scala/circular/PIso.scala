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

import cats.arrow.{Category, Split}
import cats.Eq
import cats.implicits._
import scala.annotation.tailrec

final case class PIso[A, B](to: A => Option[B], from: B => Option[A]) {
  import PIso.{associate, cons, id, iterateDriver, nil, unit}

  def lift[F[_]: PInvariant](fa: F[A]): F[B] =
    PInvariant[F].pimap(fa)(this)

  def invert: PIso[B, A] = PIso(from, to)

  def compose[C](other: PIso[C, A]): PIso[C, B] = PIso(c => other.to(c).flatMap(to), b => from(b).flatMap(other.from))

  def andThen[C](other: PIso[B, C]): PIso[A, C] = other.compose(this)

  def split[C, D](other: PIso[C, D]): PIso[(A, C), (B, D)] =
    PIso({ case (a, c) => to(a).product(other.to(c)) },
         { case (b, d) => from(b).product(other.from(d)) })

  def first[C]: PIso[(A, C), (B, C)] = split(PIso.id[C])

  def second[C]: PIso[(C, A), (C, B)] = PIso.id[C].split(this)

  def iterate(implicit ev: A =:= B): PIso[A, A] = {
    val paa = this.asInstanceOf[PIso[A, A]]
    PIso(a => Some(iterateDriver(paa.to, a)),
         a => Some(iterateDriver(paa.from, a)))
  }

  def foldLeft(implicit ev: A <:< (A, B)): PIso[(A, List[B]), A] = {
    val paba = this.asInstanceOf[PIso[(A, B), A]]
    val step: PIso[(A, List[B]), (A, List[B])] =
      id[A].split(cons[B].invert) andThen
      associate[A, B, List[B]]    andThen
      paba.split(id[List[B]])

    step.iterate.andThen(id[A].split(nil[B].invert)).andThen(unit[A].invert)
  }
}

object PIso extends PIsoInstances with PIsoFunctions {
  @tailrec
  private def iterateDriver[A](step: A => Option[A], state: A): A = step(state) match {
    case None    => state
    case Some(a) => iterateDriver(step, a)
  }
}

private[circular] sealed abstract class PIsoInstances {
  implicit val circularInstancesForPIso: Category[PIso] with Split[PIso] = new Category[PIso] with Split[PIso] {
    def compose[A, B, C](f: PIso[B, C], g: PIso[A, B]): PIso[A, C] = f.compose(g)

    def id[A]: PIso[A, A] = PIso(Some(_), Some(_))

    def split[A, B, C, D](f: PIso[A, B], g: PIso[C, D]): PIso[(A, C), (B, D)] = f.split(g)
  }
}

trait PIsoFunctions {
  def id[A]: PIso[A, A] = PIso(Some(_), Some(_))

  def fromIso[A, B](to: A => B, from: B => A): PIso[A, B] =
    PIso(a => Some(to(a)), b => Some(from(b)))

  def fromPrism[A, B](to: A => Option[B], from: B => A): PIso[A, B] = PIso(to, b => Some(from(b)))

  // PIsos for standard library - List, Vector, Option, Either

  def nil[A]: PIso[Unit, List[A]] = {
    def nilOnly(list: List[A]): Option[Unit] = list match {
      case Nil    => Some(())
      case _ :: _ => None
    }
    PIso(_ => Some(Nil), nilOnly)
  }

  def cons[A]: PIso[(A, List[A]), List[A]] = {
    def consOnly(list: List[A]): Option[(A, List[A])] = list match {
      case Nil     => None
      case a :: as => Some((a, as))
    }
    PIso( { case (a, as) => Some(a :: as) }, consOnly)
  }

  def vnil[A]: PIso[Unit, Vector[A]] = {
    def nilOnly(vector: Vector[A]): Option[Unit] = vector match {
      case Vector() => Some(())
      case _ +: _   => None
    }
    PIso(_ => Some(Vector.empty[A]), nilOnly)
  }

  def vcons[A]: PIso[(A, Vector[A]), Vector[A]] = {
    def consOnly(vector: Vector[A]): Option[(A, Vector[A])] = vector match {
      case Vector() => None
      case a +: as  => Some((a, as))
    }
    PIso( { case (a, as) => Some(a +: as) }, consOnly)
  }

  def none[A]: PIso[Unit, Option[A]] = {
    def noneOnly(opt: Option[A]): Option[Unit] = opt match {
      case None    => Some(())
      case Some(_) => None
    }
    PIso(_ => Some(None), noneOnly)
  }

  def some[A]: PIso[A, Option[A]] = PIso(a => Some(Some(a)), identity)

  def left[A, B]: PIso[A, Either[A, B]] = PIso(a => Some(Left(a)), _.left.toOption)

  def right[A, B]: PIso[B, Either[A, B]] = PIso(b => Some(Right(b)), _.right.toOption)

  def associate[A, B, C]: PIso[(A, (B, C)), ((A, B), C)] =
    PIso.fromIso({ case (a, (b, c)) => ((a, b), c) },
                 { case ((a, b), c) => (a, (b, c)) })

  def commute[A, B]: PIso[(A, B), (B, A)] =
    PIso.fromIso({ case (a, b) => (b, a) }, { case (b, a) => (a, b) })

  def unit[A]: PIso[A, (A, Unit)] = PIso.fromIso((_, ()), _._1)

  def element[A](a: A)(implicit A: Eq[A]): PIso[Unit, A] =
    PIso(_ => Some(a), a2 => if (A.eqv(a, a2)) Some(()) else None)

  def subset[A](p: A => Boolean): PIso[A, A] = {
    def f(a: A): Option[A] = if (p(a)) Some(a) else None
    PIso(f, f)
  }
}
