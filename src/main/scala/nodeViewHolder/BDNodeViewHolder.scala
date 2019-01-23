package nodeViewHolder

import akka.actor.{ActorRef, ActorSystem, Props}
import blocks.BDBlock
import scorex.core.NodeViewHolder
import scorex.core.settings.ScorexSettings
import scorex.core.utils.NetworkTimeProvider
import transaction.BDTransaction

class BDNodeViewHolder(val scorexSettings: ScorexSettings,
                       timeProvider: NetworkTimeProvider)
  extends NodeViewHolder[BDTransaction, BDBlock] {

  override type SI = BDSyncInfo
  override type HIS = BDBlockchain
  override type MS = BDState
  override type VL = BDWallet
  override type MP = BDMempool

  override def restoreState(): Option[(BDBlockchain, BDState, BDWallet, BDMempool)] = None

  override protected def genesisState: (BDBlockchain, BDState, BDWallet, BDMempool) =
    (BDBlockchain.empty, BDState.empty, BDWallet.empty, BDMempool.empty)

}


object BDNodeViewHolderRef {
  def props(settings: ScorexSettings,
            timeProvider: NetworkTimeProvider): Props =
    Props(new BDNodeViewHolder(settings, timeProvider))

  def apply(settings: ScorexSettings,
            timeProvider: NetworkTimeProvider)
           (implicit system: ActorSystem): ActorRef =
    system.actorOf(props(settings, timeProvider))

}
