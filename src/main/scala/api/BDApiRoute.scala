package api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import blocks.BDBlock
import io.circe.parser.parse
import io.circe.syntax._
import nodeViewHolder.{BDBlockchain, BDMempool, BDState, BDWallet}
import scorex.core.NodeViewHolder.CurrentView
import scorex.core.NodeViewHolder.ReceivableMessages.{GetDataFromCurrentView, LocallyGeneratedTransaction}
import scorex.core.api.http.{ApiError, ApiResponse, ApiRoute}
import scorex.core.settings.RESTApiSettings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.utils.ScorexEncoding
import scorex.crypto.signatures.PublicKey
import scorex.util.encode.Base16
import scorex.util.{ModifierId, bytesToId}
import transaction.{BDTransaction, Value}

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}


case class BDApiRoute(override val settings: RESTApiSettings, nodeViewHolderRef: ActorRef)
                     (implicit val context: ActorRefFactory, val ec: ExecutionContext)
  extends ApiRoute with ScorexEncoding {

  type PM = BDBlock
  type HIS = BDBlockchain
  type MP = BDMempool
  type MS = BDState
  type VL = BDWallet

  override val route: Route = (pathPrefix("bd") & withCors) {
    containsModifier ~ info ~ transfer
  }

  def info: Route = (path("info") & get) {
    Try {
      val chain = Await.result((nodeViewHolderRef ? GetDataFromCurrentView[HIS, MS, VL, MP, BDBlockchain](v => v.history)).mapTo[BDBlockchain], 1.second)
      val wallet = Await.result((nodeViewHolderRef ? GetDataFromCurrentView[HIS, MS, VL, MP, BDWallet](v => v.vault)).mapTo[BDWallet], 1.second)
      val mempool = Await.result((nodeViewHolderRef ? GetDataFromCurrentView[HIS, MS, VL, MP, BDMempool](v => v.pool)).mapTo[BDMempool], 1.second)
      Seq(
        "address" -> wallet.secret.publicImage.address.asJson,
        "balance" -> wallet.balance.asJson,
        "bestBlock" -> chain.bestBlock.toString.asJson,
        "unconfirmed" ->  mempool.size.toString.asJson,
        "height" -> chain.height().asJson
      )
    } match {
      case Success(resp) => ApiResponse(resp:_*)
      case Failure(e) => ApiError(e)
    }
  }

  def transfer: Route = (post & path("transfer")) {
    entity(as[String]) { body =>
      withAuth {
        parse(body) match {
          case Left(failure) => ApiError(failure.getCause)
          case Right(json) => Try {
            val amount: Value = Value @@ (json \\ "amount").head.asNumber.get.toLong.get
            val recipient: String = (json \\ "recipient").head.asString.get
            val wallet = Await.result((nodeViewHolderRef ? GetDataFromCurrentView[HIS, MS, VL, MP, BDWallet](v => v.vault)).mapTo[BDWallet], 1.second)
            val prop: PublicKey25519Proposition = PublicKey25519Proposition.validPubKey(recipient).get
            val tx = wallet.generateTx(amount, prop).get
            nodeViewHolderRef ! LocallyGeneratedTransaction[BDTransaction](tx)
            ("tx", tx.toString)
          } match {
            case Success(resp) => ApiResponse(resp)
            case Failure(e) =>
              e.printStackTrace()
              ApiError(e)
          }
        }
      }
    }
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
