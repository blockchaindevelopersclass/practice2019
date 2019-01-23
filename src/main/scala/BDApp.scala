import akka.actor.{ActorRef, Props}
import blocks.BDBlock
import mining.BDMiner.MineBlock
import mining.BDMinerRef
import nodeView.BDNodeViewSynchronizer
import nodeViewHolder._
import scorex.core.api.http.{ApiRoute, NodeViewApiRoute, PeersApiRoute, UtilsApiRoute}
import scorex.core.app.Application
import scorex.core.network.PeerFeature
import scorex.core.network.message.MessageSpec
import scorex.core.settings.ScorexSettings
import transaction.BDTransaction

import scala.concurrent.duration._
import scala.language.postfixOps

class BDApp(args: Seq[String]) extends {
  override implicit val settings: ScorexSettings = ScorexSettings.read(args.headOption)
} with Application {
  override type TX = BDTransaction
  override type PMOD = BDBlock
  override type NVHT = BDNodeViewHolder

  override protected val features: Seq[PeerFeature] = Seq()
  override protected lazy val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq(BDSyncInfoMessageSpec)

  override val nodeViewHolderRef: ActorRef = BDNodeViewHolderRef(settings, timeProvider)

  override val nodeViewSynchronizer: ActorRef =
    actorSystem.actorOf(Props(new BDNodeViewSynchronizer(networkControllerRef, nodeViewHolderRef,
      BDSyncInfoMessageSpec, settings.network, timeProvider)))

  override val swaggerConfig: String = ""
  override val apiRoutes: Seq[ApiRoute] = Seq(
    UtilsApiRoute(settings.restApi),
//    NodeViewApiRoute[TX](settings.restApi, nodeViewHolderRef),
    PeersApiRoute(peerManagerRef, networkControllerRef, timeProvider, settings.restApi)
  )

  if (settings.network.nodeName.contains("mining-node")) {
    val miner = BDMinerRef(nodeViewHolderRef, timeProvider)
    actorSystem.scheduler.scheduleOnce(10.second) {
      miner ! MineBlock(0)
    }
  }
}


object BDApp {

  def main(args: Array[String]): Unit = new BDApp(args).run()
}