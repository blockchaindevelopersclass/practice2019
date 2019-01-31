package nodeViewHolder

import blocks.BDBlock
import scorex.core.VersionTag
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.core.transaction.wallet.Vault
import scorex.util.ScorexLogging
import transaction.{BDOutput, BDTransaction}

import scala.util.Try

case class BDWallet(secret: PrivateKey25519, boxes: Seq[BDOutput])
  extends Vault[BDTransaction, BDBlock, BDWallet] with ScorexLogging {

  override type NVCT = BDWallet

  // we don't care about transactions in mempool
  override def scanOffchain(tx: BDTransaction): BDWallet = this

  // we don't care about transactions in mempool
  override def scanOffchain(txs: Seq[BDTransaction]): BDWallet = this

  override def scanPersistent(modifier: BDBlock): BDWallet = {
    val txs = modifier.transactions
    val spentIds = txs.flatMap(_.inputs)
    val remainingBoxes = boxes.filter(b => !spentIds.exists(_ sameElements b.id))
    val newBoxes = txs.flatMap(_.outputs).filter(_.proposition == secret.publicImage)
    BDWallet(secret, remainingBoxes ++ newBoxes)
  }

  override def rollback(to: VersionTag): Try[BDWallet] = Try {
    // todo not implemented
    this
  }

}

object BDWallet {
  val empty: BDWallet = {
    //TODO should read seed from config
    val secret = PrivateKey25519Companion.generateKeys("secret founders seed".getBytes())._1
    BDWallet(secret, Seq.empty)
  }
}