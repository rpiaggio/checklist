package checklist

import cats.{Applicative, Id, Monoid, Traverse}
import cats.data.Ior
import cats.implicits._
import monocle.PLens

import scala.language.higherKinds
import scala.util.matching.Regex
import Message.errors
//import cats.arrow.Profunctor
import cats.data.NonEmptyList
import checklist.SizeableSyntax._

/**
  * A Rule validates/sanitizes a value of type `A` producing a type `B` inside an `Ior[NonEmptyList[Messages], B]`
  *
  * @tparam A The type to be validated
  * @tparam B The type to be produced
  */
sealed abstract class Rule[A, B, F[_] : Applicative] {
  /**
    * Performs validation on `A`
    *
    * @tparam A The value to be validated
    */
  def apply(value: A): F[Checked[B]]

  def map[C](func: B => C): Rule[A, C, F] =
    Rule.pure(value => this (value).map(_.map(func)))

  /**
    * Maps the result type with the potential for a failure to occur.
    */
  def emap[C](func: B => Checked[C]): Rule[A, C, F] =
    Rule.pure(value => this (value).map(_.flatMap(func)))

  def recover(func: Messages => Checked[B]): Rule[A, B, F] =
    Rule.pure(value => this (value).map(_.fold(func, Ior.right, Ior.both)))

  def mapMessages(func: Messages => Messages): Rule[A, B, F] =
    Rule.pure(value => this (value).map(_.fold(func andThen Ior.left, Ior.right, (msgs, r) => Ior.both(func(msgs), r))))

  def mapEachMessage(func: Message => Message): Rule[A, B, F] =
    mapMessages(_.map(func))

  def contramap[C](func: C => A): Rule[C, B, F] =
    Rule.pure(value => this (func(value)))

  def contramapPath[C, D: PathPrefix](path: D)(func: C => A): Rule[C, B, F] =
    contramap(func).mapEachMessage(_.prefix(path))

  //  We need flatMap on F! SHould we require ev <:< FlatMap[F] ?
  //  def flatMap[C](func: B => Rule[A, C, F]): Rule[A, C, F] =
  //    Rule.pure(value => this(value) flatMap (func(_)(value)))

  //  def andThen[C](that: Rule[B, C, F]): Rule[A, C, F] =
  //    Rule.pure(value => this(value) flatMap (that.apply))
  /*
    def zip[C](that: Rule[A, C, F]): Rule[A, (B, C), F] =
      Rule.pure { a =>
        this(a) match {
          case Ior.Left(msg1) =>
            that(a) match {
              case Ior.Left(msg2)    => Ior.left(msg1 concatNel msg2)
              case Ior.Both(msg2, _) => Ior.left(msg1 concatNel msg2)
              case Ior.Right(_)      => Ior.left(msg1)
            }
          case Ior.Both(msg1, b) =>
            that(a) match {
              case Ior.Left(msg2)    => Ior.left(msg1 concatNel msg2)
              case Ior.Both(msg2, c) => Ior.both(msg1 concatNel msg2, (b, c))
              case Ior.Right(c)      => Ior.both(msg1, (b, c))
            }
          case Ior.Right(b) =>
            that(a) match {
              case Ior.Left(msg2)    => Ior.left(msg2)
              case Ior.Both(msg2, c) => Ior.both(msg2, (b, c))
              case Ior.Right(c)      => Ior.right((b, c))
            }
        }
      }
      */

  def seq[S[_] : Traverse]: Rule[S[A], S[B], F] =
    Rule.sequence(this)

  def opt: Rule[Option[A], Option[B], F] =
    Rule.optional(this)

  def req: Rule[Option[A], B, F] =
    Rule.required(this)

  def prefix[P: PathPrefix](prefix: P): Rule[A, B, F] =
    mapEachMessage(_.prefix(prefix))

  def composeLens[S, T](lens: PLens[S, T, A, B]): Rule[S, T, F] =
    Rule.pure(value => this (lens.get(value)).map(_ map (lens.set(_)(value))))

  def at[P: PathPrefix, S, T](prefix: P, lens: PLens[S, T, A, B]): Rule[S, T, F] =
    this composeLens lens prefix prefix

  // Need to think this more thoroughly
  //  def kleisli: Kleisli[Checked, A, B] = Kleisli(apply)
}

object Rule extends BaseRules
  with ConverterRules
  with PropertyRules
  with CollectionRules
  with RuleInstances
  with Rule1Syntax

trait BaseRules {
  def apply[A, F[_] : Applicative]: Rule[A, A, F] =
    pure(in => Applicative[F].pure(Ior.right(in)))

  def pure[A, B, F[_] : Applicative](func: A => F[Checked[B]]): Rule[A, B, F] =
    new Rule[A, B, F] {
      def apply(value: A) =
        func(value)
    }

  //  def fromKleisli[A, B](func: Kleisli[Checked, A, B]): Rule[A, B] = pure(func.apply)

  def pass[A, F[_] : Applicative]: Rule[A, A, F] =
    apply

  def fail[A, F[_] : Applicative](messages: Messages): Rule[A, A, F] =
    pure(in => Applicative[F].pure(Ior.both(messages, in)))
}

/** Rules that convert one type to another. */
trait ConverterRules {
  self: BaseRules =>

  val parseInt: Rule[String, Int, Id] =
    parseInt(errors("Must be a whole number"))

  def parseInt(messages: Messages): Rule[String, Int, Id] =
    pure[String, Int, Id](value => util.Try(value.toInt).toOption.map(Ior.right).getOrElse(Ior.left(messages)))

  val parseDouble: Rule[String, Double, Id] =
    parseDouble(errors("Must be a number"))

  def parseDouble(messages: Messages): Rule[String, Double, Id] =
    pure[String, Double, Id](value => util.Try(value.toDouble).toOption.map(Ior.right).getOrElse(Ior.left(messages)))

  val trimString: Rule[String, String, Id] =
    pure[String, String, Id](value => Ior.right(value.trim))
}

/** Rules that test a property of an existing value. */
trait PropertyRules {
  self: BaseRules =>

  def test[A, F[_] : Applicative](messages: => Messages, strict: Boolean = false)(func: A => F[Boolean]): Rule[A, A, F] =
    pure(value => func(value).map { result =>
      if (result) Ior.right(value) else {
        if (strict) Ior.left(messages)
        else Ior.both(messages, value)
      }
    })

  def testStrict[A, F[_] : Applicative](messages: => Messages)(func: A => F[Boolean]): Rule[A, A, F] =
    test(messages, strict = true)(func)

  def eql[A](comp: A): Rule[A, A, Id] =
    eql(comp, errors(s"Must be ${comp}"))

  def eql[A](comp: A, messages: Messages): Rule[A, A, Id] =
    test[A, Id](messages)(_ == comp)

  def eqlStrict[A](comp: A): Rule[A, A, Id] =
    eqlStrict(comp, errors(s"Must be ${comp}"))

  def eqlStrict[A](comp: A, messages: Messages): Rule[A, A, Id] =
    testStrict[A, Id](messages)(_ == comp)

  def neq[A](comp: A): Rule[A, A, Id] =
    neq[A](comp: A, errors(s"Must not be ${comp}"))

  def neq[A](comp: A, messages: Messages): Rule[A, A, Id] =
    test[A, Id](messages)(_ != comp)

  def neqStrict[A](comp: A): Rule[A, A, Id] =
    neqStrict[A](comp: A, errors(s"Must not be ${comp}"))

  def neqStrict[A](comp: A, messages: Messages): Rule[A, A, Id] =
    testStrict[A, Id](messages)(_ != comp)

  def gt[A](comp: A)(implicit ord: Ordering[_ >: A]): Rule[A, A, Id] =
    gt(comp, errors(s"Must be greater than ${comp}"))

  def gt[A](comp: A, messages: Messages)(implicit ord: Ordering[_ >: A]): Rule[A, A, Id] =
    test[A, Id](messages)(ord.gt(_, comp))

  def gtStrict[A](comp: A)(implicit ord: Ordering[A]): Rule[A, A, Id] =
    gtStrict(comp, errors(s"Must be greater than ${comp}"))

  def gtStrict[A](comp: A, messages: Messages)(implicit ord: Ordering[_ >: A]): Rule[A, A, Id] =
    testStrict[A, Id](messages)(ord.gt(_, comp))

  def lt[A](comp: A)(implicit ord: Ordering[_ >: A]): Rule[A, A, Id] =
    lt(comp, errors(s"Must be less than ${comp}"))

  def lt[A](comp: A, messages: Messages)(implicit ord: Ordering[_ >: A]): Rule[A, A, Id] =
    test[A, Id](messages)(ord.lt(_, comp))

  def ltStrict[A](comp: A)(implicit ord: Ordering[_ >: A]): Rule[A, A, Id] =
    ltStrict(comp, errors(s"Must be less than ${comp}"))

  def ltStrict[A](comp: A, messages: Messages)(implicit ord: Ordering[_ >: A]): Rule[A, A, Id] =
    testStrict[A, Id](messages)(ord.lt(_, comp))

  def gte[A](comp: A)(implicit ord: Ordering[_ >: A]): Rule[A, A, Id] =
    gte(comp, errors(s"Must be greater than or equal to ${comp}"))

  def gte[A](comp: A, messages: Messages)(implicit ord: Ordering[_ >: A]): Rule[A, A, Id] =
    test[A, Id](messages)(ord.gteq(_, comp))

  def gteStrict[A](comp: A)(implicit ord: Ordering[_ >: A]): Rule[A, A, Id] =
    gteStrict(comp, errors(s"Must be greater than or equal to ${comp}"))

  def gteStrict[A](comp: A, messages: Messages)(implicit ord: Ordering[_ >: A]): Rule[A, A, Id] =
    testStrict[A, Id](messages)(ord.gteq(_, comp))

  def lte[A](comp: A)(implicit ord: Ordering[_ >: A]): Rule[A, A, Id] =
    lte(comp, errors(s"Must be less than or equal to ${comp}"))

  def lte[A](comp: A, messages: Messages)(implicit ord: Ordering[_ >: A]): Rule[A, A, Id] =
    test[A, Id](messages)(ord.lteq(_, comp))

  def lteStrict[A](comp: A)(implicit ord: Ordering[_ >: A]): Rule[A, A, Id] =
    lteStrict(comp, errors(s"Must be less than or equal to ${comp}"))

  def lteStrict[A](comp: A, messages: Messages)(implicit ord: Ordering[_ >: A]): Rule[A, A, Id] =
    testStrict[A, Id](messages)(ord.lteq(_, comp))

  def nonEmpty[S: Monoid]: Rule[S, S, Id] =
    nonEmpty(errors(s"Must not be empty"))

  def nonEmpty[S: Monoid](messages: Messages): Rule[S, S, Id] =
    test[S, Id](messages)(value => value != Monoid[S].empty)

  def nonEmptyStrict[S: Monoid]: Rule[S, S, Id] =
    nonEmptyStrict(errors(s"Must not be empty"))

  def nonEmptyStrict[S: Monoid](messages: Messages): Rule[S, S, Id] =
    testStrict[S, Id](messages)(value => value != Monoid[S].empty)

  def lengthEq[A: Sizeable](comp: Int): Rule[A, A, Id] =
    lengthEq(comp, errors(s"Must be length ${comp}"))

  def lengthEq[A: Sizeable](comp: Int, messages: Messages): Rule[A, A, Id] =
    test[A, Id](messages)(_.size == comp)

  def lengthEqStrict[A: Sizeable](comp: Int): Rule[A, A, Id] =
    lengthEqStrict(comp, errors(s"Must be length ${comp}"))

  def lengthEqStrict[A: Sizeable](comp: Int, messages: Messages): Rule[A, A, Id] =
    testStrict[A, Id](messages)(_.size == comp)

  def lengthLt[A: Sizeable](comp: Int): Rule[A, A, Id] =
    lengthLt(comp, errors(s"Must be shorter than length ${comp}"))

  def lengthLt[A: Sizeable](comp: Int, messages: Messages): Rule[A, A, Id] =
    test[A, Id](messages)(_.size < comp)

  def lengthLtStrict[A: Sizeable](comp: Int): Rule[A, A, Id] =
    lengthLtStrict(comp, errors(s"Must be shorter than length ${comp}"))

  def lengthLtStrict[A: Sizeable](comp: Int, messages: Messages): Rule[A, A, Id] =
    testStrict[A, Id](messages)(_.size < comp)

  def lengthGt[A: Sizeable](comp: Int): Rule[A, A, Id] =
    lengthGt(comp, errors(s"Must be longer than length ${comp}"))

  def lengthGt[A: Sizeable](comp: Int, messages: Messages): Rule[A, A, Id] =
    test[A, Id](messages)(_.size > comp)

  def lengthGtStrict[A: Sizeable](comp: Int): Rule[A, A, Id] =
    lengthGtStrict(comp, errors(s"Must be longer than length ${comp}"))

  def lengthGtStrict[A: Sizeable](comp: Int, messages: Messages): Rule[A, A, Id] =
    testStrict[A, Id](messages)(_.size > comp)

  def lengthLte[A: Sizeable](comp: Int): Rule[A, A, Id] =
    lengthLte(comp, errors(s"Must be length ${comp} or shorter"))

  def lengthLte[A: Sizeable](comp: Int, messages: Messages): Rule[A, A, Id] =
    test[A, Id](messages)(_.size <= comp)

  def lengthLteStrict[A: Sizeable](comp: Int): Rule[A, A, Id] =
    lengthLteStrict(comp, errors(s"Must be length ${comp} or shorter"))

  def lengthLteStrict[A: Sizeable](comp: Int, messages: Messages): Rule[A, A, Id] =
    testStrict[A, Id](messages)(_.size <= comp)

  def lengthGte[A: Sizeable](comp: Int): Rule[A, A, Id] =
    lengthGte(comp, errors(s"Must be length ${comp} or longer"))

  def lengthGte[A: Sizeable](comp: Int, messages: Messages): Rule[A, A, Id] =
    test[A, Id](messages)(_.size >= comp)

  def lengthGteStrict[A: Sizeable](comp: Int): Rule[A, A, Id] =
    lengthGteStrict(comp, errors(s"Must be length ${comp} or longer"))

  def lengthGteStrict[A: Sizeable](comp: Int, messages: Messages): Rule[A, A, Id] =
    testStrict[A, Id](messages)(_.size >= comp)

  def nonEmptyList[A]: Rule[List[A], NonEmptyList[A], Id] =
    nonEmptyList(errors("Must not be empty"))

  def nonEmptyList[A](messages: Messages): Rule[List[A], NonEmptyList[A], Id] =
    Rule.pure[List[A], NonEmptyList[A], Id] {
      case Nil => Ior.left(messages)
      case h :: t => Ior.right(NonEmptyList(h, t))
    }

  def matchesRegex(regex: Regex): Rule[String, String, Id] =
    matchesRegex(regex, errors(s"Must match the pattern '${regex}'"))

  def matchesRegex(regex: Regex, messages: Messages): Rule[String, String, Id] =
    test[String, Id](messages)(regex.findFirstIn(_).isDefined)

  def matchesRegexStrict(regex: Regex): Rule[String, String, Id] =
    matchesRegexStrict(regex, errors(s"Must match the pattern '${regex}'"))

  def matchesRegexStrict(regex: Regex, messages: Messages): Rule[String, String, Id] =
    testStrict[String, Id](messages)(regex.findFirstIn(_).isDefined)

  def containedIn[A](values: Seq[A]): Rule[A, A, Id] =
    containedIn(values, errors(s"Must be one of the values ${values.mkString(", ")}"))

  def containedIn[A](values: Seq[A], messages: Messages): Rule[A, A, Id] =
    test[A, Id](messages)(value => values contains value)

  def containedInStrict[A](values: Seq[A]): Rule[A, A, Id] =
    containedInStrict(values, errors(s"Must be one of the values ${values.mkString(", ")}"))

  def containedInStrict[A](values: Seq[A], messages: Messages): Rule[A, A, Id] =
    testStrict[A, Id](messages)(value => values contains value)

  def notContainedIn[A](values: Seq[A]): Rule[A, A, Id] =
    notContainedIn(values, errors(s"Must not be one of the values ${values.mkString(", ")}"))

  def notContainedIn[A](values: Seq[A], messages: Messages): Rule[A, A, Id] =
    test[A, Id](messages)(value => !(values contains value))

  def notContainedInStrict[A](values: Seq[A]): Rule[A, A, Id] =
    notContainedInStrict(values, errors(s"Must not be one of the values ${values.mkString(", ")}"))

  def notContainedInStrict[A](values: Seq[A], messages: Messages): Rule[A, A, Id] =
    testStrict[A, Id](messages)(value => !(values contains value))
}

trait CollectionRules {
  self: BaseRules =>

  def optional[A, B, F[_] : Applicative](rule: Rule[A, B, F]): Rule[Option[A], Option[B], F] =
    pure {
      case Some(value) => rule(value).map(_ map (Some(_)))
      case None => Applicative[F].pure(Ior.right(None))
    }

  def required[A, B, F[_] : Applicative](rule: Rule[A, B, F]): Rule[Option[A], B, F] =
    required(rule, errors("Value is required"))

  def required[A, B, F[_] : Applicative](rule: Rule[A, B, F], messages: Messages): Rule[Option[A], B, F] =
    pure {
      case Some(value) => rule(value)
      case None => Applicative[F].pure(Ior.left(messages))
    }

  def sequence[S[_] : Traverse, A, B, F[_] : Applicative](rule: Rule[A, B, F]): Rule[S[A], S[B], F] =
    pure { values =>
        values.zipWithIndex.traverse{ case (value, index) =>
          rule.prefix(index).apply(value)
        }.map(_.sequence)
    }

  def mapValue[A: PathPrefix, B, F[_]: Applicative](key: A): Rule[Map[A, B], B, F] =
    mapValue[A, B, F](key, errors(s"Value not found"))

  def mapValue[A: PathPrefix, B, F[_]: Applicative](key: A, messages: Messages): Rule[Map[A, B], B, F] =
    pure(map => Applicative[F].pure(map.get(key).map(Ior.right).getOrElse(Ior.left(messages map (_ prefix key)))))

/*  def mapValues[A: PathPrefix, B, C, F[_]: Applicative](rule: Rule[B, C, F]): Rule[Map[A, B], Map[A, C], F] =
    pure { in: Map[A, B] =>
      in.toList.traverse {
        case (key, value) =>
          rule.prefix(key).apply(value).map(key -> _)
      }
    } map (_.toMap)*/
}

/** Type class instances for Rule */

trait RuleInstances {
  self: BaseRules =>
  /*
    implicit def ruleApplicative[A]: Applicative[Rule[A, ?]] =
      new Applicative[Rule[A, ?]] {
        def pure[B](value: B): Rule[A, B] =
          Rule.pure(_ => Ior.right(value))

        def ap[B, C](funcRule: Rule[A, B => C])(argRule: Rule[A, B]): Rule[A, C] =
          (funcRule zip argRule) map { pair =>
            val (func, arg) = pair
            func(arg)
          }

        override def map[B, C](rule: Rule[A, B])(func: B => C): Rule[A, C] =
          rule map func

        override def product[B, C](rule1: Rule[A, B], rule2: Rule[A, C]): Rule[A, (B, C)] =
          rule1 zip rule2
      }
  */
  //  implicit val ruleProfunctor: Profunctor[Rule] =
  //    new Profunctor[Rule] {
  //      override def dimap[A, B, C, D](fab: Rule[A, B])(f: (C) => A)(g: (B) => D) =
  //        fab.contramap(f).map(g)
  //    }
}
