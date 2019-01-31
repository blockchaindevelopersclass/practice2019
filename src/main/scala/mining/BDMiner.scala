package mining

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import blocks.BDBlock
import mining.BDMiner.MineBlock
import nodeViewHolder.{BDBlockchain, BDMempool}
import scorex.core.NodeViewHolder.ReceivableMessages.LocallyGeneratedModifier
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{ChangedHistory, ChangedMempool, SemanticallySuccessfulModifier}
import scorex.core.utils.NetworkTimeProvider
import scorex.util.ScorexLogging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.math.BigInt

class BDMiner(viewHolderRef: ActorRef, timeProvider: NetworkTimeProvider) extends Actor with ScorexLogging {

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[ChangedMempool[_]])
    context.system.eventStream.subscribe(self, classOf[ChangedHistory[_]])
  }

  var currentMempool: BDMempool = new BDMempool
  var currentCandidate: BDBlock = constructNewBlock(BDBlockchain.GenesisBlock)

  override def receive: Receive = {
    case ChangedHistory(h: BDBlockchain@unchecked) =>
      currentCandidate = constructNewBlock(h.bestBlock)

    case ChangedMempool(pool: BDMempool) =>
      currentMempool = pool

    case MineBlock(newNonce) =>
      val newBlock = currentCandidate.copy(nonce = newNonce)
      if (BDMiner.correctWorkDone(newBlock)) {
        log.info(s"New block ${newBlock.encodedId} found")
        viewHolderRef ! LocallyGeneratedModifier(newBlock)
      }
      context.system.scheduler.scheduleOnce(10.second) {
        self ! MineBlock(newNonce + 1)
      }

    case m => log.warn(s"Unexpected message $m")
  }

  private def constructNewBlock(parent: BDBlock): BDBlock = {
    val transactions = currentMempool.take(1)
    val target = parent.currentTarget
    BDBlock(transactions,
      parent.id,
      target,
      0,
      0: Byte,
      timeProvider.time())
  }

}


object BDMiner {

  case class MineBlock(nonce: Long)

  val MaxTarget: Long = Long.MaxValue

  def correctWorkDone(block: BDBlock): Boolean = {
    realDifficulty(block) <= block.currentTarget
  }

  private def realDifficulty(block: BDBlock): BigInt = MaxTarget / BigInt(1, block.hash)

}

object BDMinerRef {
  def props(viewHolderRef: ActorRef, timeProvider: NetworkTimeProvider): Props =
    Props(new BDMiner(viewHolderRef: ActorRef, timeProvider: NetworkTimeProvider))

  def apply(viewHolderRef: ActorRef, timeProvider: NetworkTimeProvider)
           (implicit system: ActorSystem): ActorRef =
    system.actorOf(props(viewHolderRef, timeProvider))

}