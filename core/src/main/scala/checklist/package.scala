import cats.data.{Ior, NonEmptyList}

import scala.language.higherKinds

package object checklist {
  type Messages       = NonEmptyList[Message]
  type Checked[A]     = Messages Ior A
  type Rule1[A, F[_]] = Rule[A, A, F]
}
