import akka.actor.{ActorRef, Props}
import api.BDApiRoute
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
import scala.io.Source
import scala.language.postfixOps

class BDApp(configPath: String) extends {
  override implicit val settings: ScorexSettings = ScorexSettings.read(Some(configPath))
  override protected val features: Seq[PeerFeature] = Seq()
} with Application {
  override type TX = BDTransaction
  override type PMOD = BDBlock
  override type NVHT = BDNodeViewHolder

  override protected lazy val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq(BDSyncInfoMessageSpec)

  override val nodeViewHolderRef: ActorRef = BDNodeViewHolderRef(settings, timeProvider)

  override val nodeViewSynchronizer: ActorRef =
    actorSystem.actorOf(Props(new BDNodeViewSynchronizer(networkControllerRef, nodeViewHolderRef,
      BDSyncInfoMessageSpec, settings.network, timeProvider)))

  override val swaggerConfig: String = Source.fromResource("api.yaml").getLines.mkString("\n")

  override val apiRoutes: Seq[ApiRoute] = Seq(
    UtilsApiRoute(settings.restApi),
    BDApiRoute(settings.restApi, nodeViewHolderRef),
    PeersApiRoute(peerManagerRef, networkControllerRef, timeProvider, settings.restApi)
  )

  if (settings.network.nodeName.contains("mining-node")) {
    val miner = BDMinerRef(nodeViewHolderRef, timeProvider)
    actorSystem.scheduler.scheduleOnce(5.minute) {
      miner ! MineBlock(0)
    }
  }
}


object BDApp {

  def main(args: Array[String]): Unit = {
    new BDApp(args.headOption.getOrElse("src/main/resources/node1.conf")).run()
  }
}