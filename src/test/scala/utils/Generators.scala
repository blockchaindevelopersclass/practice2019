package utils

import blocks.BDBlock
import nodeViewHolder.BDSyncInfo
import org.scalacheck.{Arbitrary, Gen}
import scorex.core.block.Block.Version
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.crypto.signatures.{PublicKey, Signature}
import scorex.testkit.generators.CoreGenerators
import scorex.util.ModifierId
import transaction._

trait Generators extends CoreGenerators {

  val preimageProofGenerator: Gen[Signature25519] = nonEmptyBytesGen
    .map(b => Signature25519(Signature @@ b))

  val preimagePropositionGenerator: Gen[PublicKey25519Proposition] = nonEmptyBytesGen
    .map(b => PublicKey25519Proposition(PublicKey @@ b))

  val outputGen: Gen[Output] = for {
    p <- preimagePropositionGenerator
    value <- positiveLongGen
  } yield Output(p, Value @@ value)

  val BDTransactionGenerator: Gen[BDTransaction] = for {
    inputs <- Gen.nonEmptyListOf(genBytes(32)).map(b => OutputId @@ b)
    outputs <- Gen.nonEmptyListOf(outputGen)
    signatures <- Gen.listOfN(inputs.length, preimageProofGenerator)
  } yield BDTransaction(inputs.toIndexedSeq, outputs.toIndexedSeq, signatures.toIndexedSeq)

  val BDBlockGenerator: Gen[BDBlock] = for {
    parentId <- modifierIdGen
    transactions <- smallInt.flatMap(n => Gen.listOfN(n, BDTransactionGenerator))
    currentTarget <- positiveLongGen
    nonce <- positiveLongGen
    timestamp <- positiveLongGen
    version <- Arbitrary.arbitrary[Byte]
  } yield BDBlock(transactions: Seq[BDTransaction],
    parentId: ModifierId,
    currentTarget: Long,
    nonce: Long,
    version: Version,
    timestamp: Long)

  val BDSyncInfoGen: Gen[BDSyncInfo] = for {
    ids <- Gen.nonEmptyListOf(modifierIdGen)
  } yield BDSyncInfo(ids)

}
