package utils

import blocks.BDBlock
import com.google.common.primitives.Ints
import nodeViewHolder.BDSyncInfo
import org.scalacheck.{Arbitrary, Gen}
import scorex.core.block.Block.Version
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.crypto.signatures.{PublicKey, Signature}
import scorex.testkit.generators.CoreGenerators
import scorex.util.ModifierId
import transaction._

trait Generators extends CoreGenerators {

  lazy val skPkGen: Gen[(PrivateKey25519, PublicKey25519Proposition)] = genBytes(32).map(b => PrivateKey25519Companion.generateKeys(b))

  lazy val pkPropositionGen: Gen[PublicKey25519Proposition] = skPkGen.map(_._2)

  lazy val signatureGen: Gen[Signature25519] = genBytes(64)
    .map(b => Signature25519(Signature @@ b))

  lazy val outputGen: Gen[BDOutput] = for {
    p <- pkPropositionGen
    value <- positiveLongGen
  } yield BDOutput(p, Value @@ value)

  lazy val BDTransactionGenerator: Gen[BDTransaction] = for {
    inputs <- Gen.nonEmptyListOf(genBytes(32)).map(b => OutputId @@ b)
    outputs <- Gen.nonEmptyListOf(outputGen)
    signatures <- Gen.listOfN(inputs.length, signatureGen)
  } yield BDTransaction(inputs.toIndexedSeq, outputs.toIndexedSeq, signatures.toIndexedSeq)

  lazy val BDBlockGenerator: Gen[BDBlock] = for {
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

  lazy val BDSyncInfoGen: Gen[BDSyncInfo] = for {
    ids <- Gen.nonEmptyListOf(modifierIdGen)
  } yield BDSyncInfo(ids)

}
