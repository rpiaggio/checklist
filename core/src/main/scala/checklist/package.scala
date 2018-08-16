import cats.Id
import cats.data.{Ior, NonEmptyList}

import scala.language.higherKinds

package object checklist {
  type Messages         = NonEmptyList[Message]
  type Checked[A]       = Messages Ior A
  type Rule1[F[_], A]   = Rule[F, A, A]
  type Check[F[_], A]   = Rule[F, A, A]
  type CheckNow[A]      = Check[Id, A]
}
