package transaction

import nodeViewHolder.BDMempool
import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.ModifierId
import scorex.testkit.generators.CoreGenerators
import scorex.testkit.properties.mempool.MempoolTransactionsTest
import utils.Generators

class BDMempoolTest extends PropSpec
  with Generators
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with CoreGenerators
  with MempoolTransactionsTest[BDTransaction, BDMempool] {

  override val memPoolGenerator: Gen[BDMempool] = new BDMempool
  override val memPool: BDMempool = new BDMempool

  override val transactionGenerator: Gen[BDTransaction] = BDTransactionGenerator
  override val transactionSeqGenerator: Gen[Seq[BDTransaction]] = Gen.nonEmptyContainerOf[Seq, BDTransaction](transactionGenerator)


  type TX = BDTransaction
  type MPool = BDMempool


}