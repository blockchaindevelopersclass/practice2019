package blocks

import scorex.core.block.Block
import scorex.core.block.Block.Version
import scorex.core.{ModifierTypeId, bytesToId, idToBytes}
import scorex.core.serialization.ScorexSerializer
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.util.ModifierId
import scorex.util.serialization.{Reader, Writer}
import transaction.{BDTransaction, BDTransactionSerializer}

case class BDBlock(transactions: Seq[BDTransaction],
                   parentId: ModifierId,
                   currentTarget: Long,
                   nonce: Long,
                   version: Version,
                   timestamp: Long) extends Block[BDTransaction] {

  override val modifierTypeId: ModifierTypeId = BDBlock.BDBlockModifierTypeId

  val hash: Digest32 = Blake2b256(BDBlockSerializer.toBytes(this))

  override val id: ModifierId = bytesToId(hash)

  override def toString: String = s"BDBlock($nonce,$id,${transactions.toList})"
}

object BDBlock {

  val BDBlockModifierTypeId: ModifierTypeId = ModifierTypeId @@ 10.toByte

}

object BDBlockSerializer extends ScorexSerializer[BDBlock] {

  override def serialize(obj: BDBlock, w: Writer): Unit = {
    w.putInt(obj.transactions.size)
    obj.transactions.foreach(tx => BDTransactionSerializer.serialize(tx, w))
    w.putBytes(idToBytes(obj.parentId))
    w.putLong(obj.currentTarget)
    w.putLong(obj.nonce)
    w.put(obj.version)
    w.putLong(obj.timestamp)
  }

  override def parse(r: Reader): BDBlock = {
    val txSize = r.getInt()
    val txs = (0 until txSize) map (_ => BDTransactionSerializer.parse(r))
    val parentId = bytesToId(r.getBytes(32))
    val target = r.getLong()
    val nonce = r.getLong()
    val version = r.getByte()
    val ts = r.getLong()
    BDBlock(txs, parentId, target, nonce, version, ts)
  }
}

