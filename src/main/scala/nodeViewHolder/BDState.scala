package nodeViewHolder

import blocks.BDBlock
import scorex.core.VersionTag
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.MinimalState
import scorex.util.ScorexLogging
import transaction.{BDOutput, Value}

import scala.util.{Failure, Try}

case class BDState(override val version: VersionTag, utxo: Seq[BDOutput]) extends MinimalState[BDBlock, BDState]
  with ScorexLogging {
  override def applyModifier(mod: BDBlock): Try[BDState] = Try {
    log.info(s"Apply block ${mod.id} with ${mod.transactions.size} transactions to state of version $version")

    val inputs = mod.transactions.flatMap(_.inputs)
    mod.transactions.foreach { tx =>
      require(tx.inputs.size == tx.signatures.size, "not enough signatures")
      require(tx.outputs.forall(_.value >= 0), "negative amount to transfer")
      val from: Seq[BDOutput] = tx.inputs.map(i => utxo.find(_.id sameElements i).get)
      require(from.map(_.value.toLong).sum == tx.outputs.map(_.value.toLong).sum, "Sum of inputs != sum of outputs")

      require(from.zip(tx.signatures).forall { case (input, proof) =>
        proof.isValid(input.proposition, tx.messageToSign)
      }, "proofs are incorrect")
    }

    val filtered = utxo.filter(o => !inputs.exists(_ sameElements o.id))

    BDState(VersionTag @@ mod.id, filtered ++ mod.transactions.flatMap(_.outputs))
  }

  override def rollbackTo(version: VersionTag): Try[BDState] = Failure(new Error("Not supported"))

  override def maxRollbackDepth: Int = 0

  override type NVCT = this.type
}

object BDState {

  val genesisState: Seq[BDOutput] = {
    Seq(
      BDOutput(PublicKey25519Proposition.validPubKey("01456a08bda264b3e6d4211f2bbb0478c4049b796afb759daace23c4247f72ea71b377262d").get, Value @@ 1000000L)
    )
  }

  val empty: BDState = BDState(VersionTag @@ BDBlockchain.GenesisBlock.id, genesisState)
}