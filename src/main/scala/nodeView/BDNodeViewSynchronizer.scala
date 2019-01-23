package nodeView

import akka.actor.ActorRef
import blocks.{BDBlock, BDBlockSerializer}
import nodeViewHolder.{BDBlockchain, BDMempool, BDSyncInfo, BDSyncInfoMessageSpec}
import scorex.core.network.NodeViewSynchronizer
import scorex.core.settings.NetworkSettings
import scorex.core.transaction.Transaction
import scorex.core.utils.NetworkTimeProvider
import transaction.{BDTransaction, BDTransactionSerializer}

import scala.concurrent.ExecutionContext

class BDNodeViewSynchronizer(networkControllerRef: ActorRef,
                             viewHolderRef: ActorRef,
                             syncInfoSpec: BDSyncInfoMessageSpec.type,
                             networkSettings: NetworkSettings,
                             timeProvider: NetworkTimeProvider)(implicit ex: ExecutionContext) extends
  NodeViewSynchronizer[BDTransaction, BDSyncInfo, BDSyncInfoMessageSpec.type, BDBlock,
  BDBlockchain, BDMempool](networkControllerRef, viewHolderRef, syncInfoSpec, networkSettings, timeProvider,
  Map(BDBlock.BDBlockModifierTypeId -> BDBlockSerializer,  Transaction.ModifierTypeId -> BDTransactionSerializer)
)
