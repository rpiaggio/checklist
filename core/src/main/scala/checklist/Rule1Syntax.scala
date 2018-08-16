package checklist

import cats.Applicative
//import monocle._

//import scala.language.experimental.macros
import scala.language.higherKinds

trait Rule1Syntax {
  implicit class AnyRuleOps[A](value: A) {
    def validate[F[_]: Applicative](implicit rule: Rule[F, A, A]): F[Checked[A]] =
      rule(value)
  }

  implicit class Rule1Ops[A, F[_]: Applicative](self: Rule[F, A, A]) {
//    def field[B](path: Path, lens: Lens[A, B])(implicit rule: Rule[F, B, B]): Rule[F, A, A] =
//      self andThen rule.at(path, lens)

//    def field[B](accessor: A => B)(implicit rule: Rule[F, B, B]): Rule[F, A, A] =
//      macro RuleMacros.field[A, B, F]

//    def fieldWith[B](path: Path, lens: Lens[A, B])(implicit builder: A => Rule[F, B, B]): Rule[F, A, A] =
//      self andThen Rule.pure(value => builder(value).at(path, lens).apply(value))

//    def fieldWith[B](accessor: A => B)(implicit builder: A => Rule[F, B, B]): Rule[F, A, A] =
//      macro RuleMacros.fieldWith[A, B, F]
  }
}
