package checklist

import cats.Applicative
import cats.implicits._
import monocle._

import scala.language.experimental.macros
import scala.language.higherKinds

trait Rule1Syntax {

  implicit class AnyRuleOps[A](value: A) {
    def validate[F[_] : Applicative](implicit rule: Rule[F, A, A]): F[Checked[A]] =
      rule(value)
  }

  implicit class Rule1Ops[F[_] : Applicative, A](self: Check[F, A]) {
    def and[G[_] : Applicative, R[_]](that: Check[G, A])(implicit canLift: CanLift[Applicative, F, G, R]): Rule[R, A, A] = {
      implicit val evApplicativeR: Applicative[R] = canLift.evR

      (self.liftWith(canLift.liftF), that.liftWith(canLift.liftG)).mapN((x, _) => x)
    }

    def field[G[_] : Applicative, R[_], B](path: Path, lens: Lens[A, B])(rule: Rule[G, B, B])(implicit canLift: CanLift[Applicative, F, G, R]): Rule[R, A, A] =
      self and rule.at(path, lens)

    def field[G[_] : Applicative, R[_], B](accessor: A => B)(rule: Rule[G, B, B])(implicit canLift: CanLift[Applicative, F, G, R]): Rule[R, A, A] =
    macro RuleMacros.field[F, G, R, A, B]

    // This is not really working, the compiler can't unify. We should try with embedded class with apply().
    def fieldImplicit[G[_] : Applicative, R[_], B](path: Path, lens: Lens[A, B])(implicit rule: Rule[G, B, B], canLift: CanLift[Applicative, F, G, R]): Rule[R, A, A] =
      field(path, lens)(rule)

    def fieldImplicit[G[_] : Applicative, R[_], B](accessor: A => B)(implicit rule: Rule[G, B, B], canLift: CanLift[Applicative, F, G, R]): Rule[R, A, A] =
    macro RuleMacros.fieldImplicit[F, G, R, A, B]

    def fieldWith[G[_] : Applicative, R[_], B](path: Path, lens: Lens[A, B])(builder: A => Rule[G, B, B])(implicit canLift: CanLift[Applicative, F, G, R]): Rule[R, A, A] =
      self and Rule.pure(value => builder(value).at(path, lens).apply(value))

    def fieldWith[G[_] : Applicative, R[_],B](accessor: A => B)(builder: A => Rule[G, B, B])(implicit canLift: CanLift[Applicative, F, G, R]): Rule[R, A, A] =
    macro RuleMacros.fieldWith[F, G, R, A, B]

    def fieldWithImplicit[G[_] : Applicative, R[_], B](path: Path, lens: Lens[A, B])(implicit builder: A => Rule[G, B, B], canLift: CanLift[Applicative, F, G, R]): Rule[R, A, A] =
        fieldWith(path, lens)(builder)

    def fieldWithImplicit[G[_] : Applicative, R[_],B](accessor: A => B)(implicit builder: A => Rule[G, B, B], canLift: CanLift[Applicative, F, G, R]): Rule[R, A, A] =
    macro RuleMacros.fieldWithImplicit[F, G, R, A, B]
  }

}
