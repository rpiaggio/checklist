package checklist

import cats.{Applicative, Monad}
import monocle._

import scala.language.experimental.macros
import scala.language.higherKinds

trait Rule1Syntax {

  implicit class AnyRuleOps[A](value: A) {
    def validate[F[_] : Applicative](implicit rule: Rule[F, A, A]): F[Checked[A]] =
      rule(value)
  }

  implicit class Rule1Ops[F[_] : Applicative, A](self: Rule[F, A, A]) {
    def field[B](path: Path, lens: Lens[A, B])(rule: Rule[F, B, B])(implicit ev: Monad[F]): Rule[F, A, A] =
      self andThen rule.at(path, lens)

    def field[B](accessor: A => B)(rule: Rule[F, B, B])(implicit ev: Monad[F]): Rule[F, A, A] =
    macro RuleMacros.field[F, A, B]

    def fieldImplicit[B](path: Path, lens: Lens[A, B])(implicit rule: Rule[F, B, B], ev: Monad[F]): Rule[F, A, A] =
      self andThen rule.at(path, lens)

    def fieldImplicit[B](accessor: A => B)(implicit rule: Rule[F, B, B], ev: Monad[F]): Rule[F, A, A] =
    macro RuleMacros.fieldImplicit[F, A, B]

    def fieldWith[B](path: Path, lens: Lens[A, B])(builder: A => Rule[F, B, B])(implicit ev: Monad[F]): Rule[F, A, A] =
      self andThen Rule.pure(value => builder(value).at(path, lens).apply(value))

    def fieldWith[B](accessor: A => B)(builder: A => Rule[F, B, B])(implicit ev: Monad[F]): Rule[F, A, A] =
    macro RuleMacros.fieldWith[F, A, B]

    def fieldWithImplicit[B](path: Path, lens: Lens[A, B])(implicit builder: A => Rule[F, B, B], ev: Monad[F]): Rule[F, A, A] =
      self andThen Rule.pure(value => builder(value).at(path, lens).apply(value))

    def fieldWithImplicit[B](accessor: A => B)(implicit builder: A => Rule[F, B, B], ev: Monad[F]): Rule[F, A, A] =
    macro RuleMacros.fieldWithImplicit[F, A, B]
  }

}
