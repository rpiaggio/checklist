package checklist

import cats._
import cats.data.Ior
import cats.implicits._
import org.scalatest.{FreeSpec, Matchers}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class ContextSpec extends FreeSpec with Matchers {

  "pass" in {
    val rule1 = Rule.pass[Int]
    val rule2 = rule1.liftTo[Future]
    val combined1 = rule1 and rule2
    val combined2 = rule2 and rule1
    val combined3 = rule1 andThen rule2
    val combined4 = rule2 andThen rule1
    combined1(+1) map (_ should be(Ior.right(+1)))
    combined2(+1) map (_ should be(Ior.right(+1)))
    combined3(+1) map (_ should be(Ior.right(+1)))
    combined4(+1) map (_ should be(Ior.right(+1)))
  }

}
