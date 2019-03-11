package nodeViewHolder

import blocks.BDBlock
import scorex.core.VersionTag
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.core.transaction.wallet.Vault
import scorex.util.ScorexLogging
import transaction.{BDOutput, BDTransaction, OutputId, Value}

import scala.util.Try

class BDWallet(val secret: PrivateKey25519, var boxes: Seq[BDOutput]) extends Vault[BDTransaction, BDBlock, BDWallet]
  with ScorexLogging {


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
    boxes = remainingBoxes ++ newBoxes
    this
  }

  def balance: Long = boxes.map(_.value.toLong).sum

  def generateTx(amount: Value, recipient: PublicKey25519Proposition): Try[BDTransaction] = Try {
    val inputs = boxes.map(t => OutputId !@@ t.id).toIndexedSeq
    val remaining = boxes.map(_.value.toLong).sum - amount
    val outputs = IndexedSeq(BDOutput(recipient, amount), BDOutput(secret.publicImage, Value @@ remaining))

    val unsigned = BDTransaction(inputs, outputs, IndexedSeq[Signature25519]())
    val msg = unsigned.messageToSign
    val signatures = inputs.map(_ => PrivateKey25519Companion.sign(secret, msg))
    BDTransaction(inputs, outputs, signatures)
  }

  override def rollback(to: VersionTag): Try[BDWallet] = Try {
    // todo not implemented
    this
  }

}

object BDWallet {

  def apply(seed: String): BDWallet = {
    val secret = PrivateKey25519Companion.generateKeys(seed.getBytes())._1
    val boxes = BDState.genesisState.filter(_.proposition == secret.publicImage)
    new BDWallet(secret, boxes)
  }

}