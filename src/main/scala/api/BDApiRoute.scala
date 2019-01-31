package api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import blocks.BDBlock
import io.circe.syntax._
import nodeViewHolder.{BDBlockchain, BDMempool, BDState, BDWallet}
import scorex.core.NodeViewHolder.CurrentView
import scorex.core.NodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import scorex.core.api.http.{ApiError, ApiResponse, ApiRoute}
import scorex.core.settings.RESTApiSettings
import scorex.core.utils.ScorexEncoding
import scorex.util.{ModifierId, bytesToId}

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}


case class BDApiRoute(override val settings: RESTApiSettings, nodeViewHolderRef: ActorRef)
                     (implicit val context: ActorRefFactory, val ec: ExecutionContext)
  extends ApiRoute with ScorexEncoding {

  type PM = BDBlock
  type HIS = BDBlockchain
  type MP = BDMempool
  type MS = BDState
  type VL = BDWallet

  override val route: Route = (pathPrefix("bd") & withCors) {
    containsModifier
  }

  def containsModifier: Route = (get & path("contains" / Segment)) { encodedId =>
    def f(v: CurrentView[HIS, MS, VL, MP]): Option[PM] = v.history.modifierById(ModifierId @@ encodedId)

    val contains = (nodeViewHolderRef ? GetDataFromCurrentView[HIS, MS, VL, MP, Option[PM]](f)).mapTo[Option[PM]]

    onComplete(contains) { r =>
      ApiResponse(
        "id" -> encodedId.asJson,
        "contains" -> r.toOption.flatten.isDefined.asJson
      )
    }
  }

}
