package nodeViewHolder

import blocks.BDBlock
import mining.BDMiner
import scorex.core.bytesToId
import scorex.core.consensus.BlockChain.Score
import scorex.core.consensus.History._
import scorex.core.consensus.{BlockChain, ModifierSemanticValidity}
import scorex.core.utils.ScorexEncoding
import scorex.core.validation.RecoverableModifierError
import scorex.util.ModifierId
import transaction.BDTransaction

import scala.util.Try

case class BDBlockchain(blocks: Map[Int, BDBlock],
                        reverseMap: Map[String, Int], isValid: Map[BDBlock, Boolean])
  extends BlockChain[BDTransaction, BDBlock, BDSyncInfo, BDBlockchain] with ScorexEncoding {

  def bestBlock: BDBlock = blocks.maxBy(_._1)._2


  override def height(): Int = blocks.keys.max

  override def heightOf(id: ModifierId): Option[Int] = reverseMap.get(id)

  override def blockAt(height: Int): Option[BDBlock] = blocks.get(height)

  override def children(blockId: ModifierId): Seq[BDBlock] = heightOf(blockId).map(_ + 1).flatMap(blockAt).toSeq

  // TODO this is simplified version
  override def score(block: BDBlock): Score = BigInt(heightOf(block).getOrElse(0))

  override def chainScore(): Score = score(bestBlock)

  override def append(block: BDBlock): Try[(BDBlockchain, ProgressInfo[BDBlock])] = Try {
    val blockHeight = height() + 1
    val progressInfo = ProgressInfo(None, Seq.empty, Seq(block), Seq.empty)
    log.info(s"Appended block ${block.id} with height $blockHeight")
    (BDBlockchain(blocks + (blockHeight -> block), reverseMap + (block.id -> blockHeight), isValid), progressInfo)
  }

  override def reportModifierIsValid(modifier: BDBlock): BDBlockchain = {
    BDBlockchain(blocks, reverseMap, isValid + (modifier -> true))
  }

  override def reportModifierIsInvalid(modifier: BDBlock, progressInfo: ProgressInfo[BDBlock]): (BDBlockchain, ProgressInfo[BDBlock]) = {
    (BDBlockchain(blocks - reverseMap(modifier.encodedId), reverseMap - modifier.encodedId, isValid + (modifier -> false)), ProgressInfo[BDBlock](None, Seq.empty, Seq.empty, Seq.empty))
  }

  override def applicableTry(block: BDBlock): Try[Unit] = Try {
    if (!BDMiner.correctWorkDone(block)) throw new Error(s"Incorrect target for ${block.encodedId}")
    //TODO forks are not supported in this implementation
    if (bestBlock.id != block.parentId) throw new RecoverableModifierError(s"Incorrect parentId for ${block.encodedId}")
  }

  override def modifierById(id: ModifierId): Option[BDBlock] = reverseMap.get(id).flatMap(h => blocks.get(h))

  override def isSemanticallyValid(id: ModifierId): ModifierSemanticValidity = reverseMap.get(id) match {
    case Some(_) => ModifierSemanticValidity.Valid
    case _ => ModifierSemanticValidity.Unknown
  }

  override def syncInfo: BDSyncInfo = {
    BDSyncInfo(lastBlockIds(BDSyncInfo.idsSize))
  }

  override def compare(other: BDSyncInfo): HistoryComparisonResult = {
    val theirIds = other.ids
    theirIds.reverse.find(id => contains(id)) match {
      case Some(common) =>
        val commonHeight = heightOf(common).get
        val theirTotalHeight = theirIds.indexWhere(_ sameElements common) + commonHeight
        val ourHeight = height()
        if (theirTotalHeight == ourHeight) {
          Equal
        } else if (theirTotalHeight > ourHeight) {
          Older
        } else {
          Younger
        }
      case _ => Unknown
    }
  }

  override type NVCT = BDBlockchain

}

object BDBlockchain {

  val GenesisBlock: BDBlock = BDBlock(Seq.empty,
    bytesToId(Array.fill(32)(0: Byte)),
    BDMiner.MaxTarget,
    0,
    0: Byte,
    1517329800000L)

  val empty: BDBlockchain = BDBlockchain(Map(1 -> GenesisBlock), Map(GenesisBlock.encodedId -> 1), Map(GenesisBlock -> true))

}
