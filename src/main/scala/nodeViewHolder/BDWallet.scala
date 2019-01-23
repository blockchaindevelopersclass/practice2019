package nodeViewHolder

import blocks.BDBlock
import com.google.common.primitives.Ints
import scorex.core.VersionTag
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.core.transaction.wallet.{BoxWallet, BoxWalletTransaction, WalletBox}
import scorex.util.ScorexLogging
import transaction.{BDTransaction, Output}

import scala.util.Try

case class BDWallet(seed: Array[Byte], secrets: Set[PrivateKey25519], boxesVar: Seq[WalletBox[PublicKey25519Proposition, Output]])
  extends BoxWallet[PublicKey25519Proposition, BDTransaction, BDBlock, BDWallet] with ScorexLogging {

  override type S = PrivateKey25519
  override type PI = PublicKey25519Proposition


  override def generateNewSecret(): BDWallet = {
    val newSecret = PrivateKey25519Companion.generateKeys(seed ++ Ints.toByteArray(secrets.size))._1
    BDWallet(seed, secrets + newSecret, boxesVar)
  }

  override def historyTransactions: Seq[BoxWalletTransaction[PublicKey25519Proposition, BDTransaction]] = ???

  override def boxes(): Seq[WalletBox[PublicKey25519Proposition, _ <: Box[PublicKey25519Proposition]]] = boxesVar

  // todo do not work with offchain
  override def scanOffchain(tx: BDTransaction): BDWallet = this

  // todo do not work with offchain
  override def scanOffchain(txs: Seq[BDTransaction]): BDWallet = this


  override def scanPersistent(modifier: BDBlock): BDWallet = {
/*
    val txs = modifier.transactions
    val spentIds = txs.flatMap(_.inputs)
    val spentBoxes = boxesVar.filter(b => !spentIds.exists(_ sameElements b.box.id))
    val newBoxes: Seq[WalletBox[PublicKey25519Proposition, Output]] = txs
      .flatMap(t => t.outputs.map(_ -> t))
      .filter(to => publicKeys.exists(_.pubKeyBytes sameElements to._1.proposition.pubKeyBytes))
      .map(b => WalletBox(b._1, b._2.id, modifier.timestamp))
      .flatten
    BDWallet(seed, secrets, spentBoxes ++ newBoxes)
*/
    this
  }

  override def rollback(to: VersionTag): Try[BDWallet] = Try {
    // todo not implemented
    this
  }

  override lazy val publicKeys: Set[PublicKey25519Proposition] = secrets.map(_.publicImage)

  override def secretByPublicImage(publicImage: PI): Option[S] = secrets.find(_.publicImage == publicImage)

  override type NVCT = this.type
}

object BDWallet {
  //TODO should read seed from config
  val empty: BDWallet = BDWallet(Array.empty, Set.empty, Seq.empty)
}