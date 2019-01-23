package blocks

import io.circe.Json
import org.msgpack.core.MessagePack
import scorex.core.ModifierTypeId
import scorex.core.block.Block
import scorex.core.block.Block.Version
import scorex.core.serialization.Serializer
import scorex.core.utils.concatBytes
import scorex.core.bytesToId
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.util.ModifierId
import scorex.util.encode.Base16
import transaction.{BDTransaction, BDTransactionSerializer}

import scala.util.Try

case class BDBlock(transactions: Seq[BDTransaction],
                   parentId: ModifierId,
                   currentTarget: Long,
                   nonce: Long,
                   version: Version,
                   timestamp: Long) extends Block[BDTransaction] {
  override type M = BDBlock

  override val modifierTypeId: ModifierTypeId = BDBlock.BDBlockModifierTypeId

  val hash: Digest32 = Blake2b256(bytes)

  override val id: ModifierId = bytesToId(hash)

  override def serializer: Serializer[BDBlock] = BDBlockSerializer
}

object BDBlock {

  val BDBlockModifierTypeId: ModifierTypeId = ModifierTypeId @@ 10.toByte

}

object BDBlockSerializer extends Serializer[BDBlock] {

  private def seqToBytes[A](sequence: IndexedSeq[A], mapping: A => Array[Byte]): Array[Byte] =
    if (sequence.nonEmpty) concatBytes(sequence.map(mapping)) else Array[Byte]()

  override def toBytes(obj: BDBlock): Array[Version] = {
    val packer = MessagePack.newDefaultBufferPacker()
    packer.packBinaryHeader(obj.parentId.length)
    packer.writePayload(Base16.decode(obj.parentId).get)
    packer.packArrayHeader(obj.transactions.size)
    for {
      input <- obj.transactions
    } yield {
      val payload = BDTransactionSerializer.toBytes(input)
      packer.packBinaryHeader(payload.length)
      packer.writePayload(payload)
    }
    packer.packLong(obj.currentTarget)
    packer.packLong(obj.nonce)
    packer.packLong(obj.timestamp)
    packer.packByte(obj.version)
    packer.toByteArray
  }

  override def parseBytes(bytes: Array[Version]): Try[BDBlock] = Try {
    val unpacker = MessagePack.newDefaultUnpacker(bytes)
    val idSize = unpacker.unpackBinaryHeader()
    val parentId = bytesToId(unpacker.readPayload(idSize))
    val txsNum = unpacker.unpackArrayHeader()
    val txs = for {
      i <- Range(0, txsNum)
    } yield {
      val binaryLen = unpacker.unpackBinaryHeader()
      val txBytes = unpacker.readPayload(binaryLen)
      BDTransactionSerializer.parseBytes(txBytes).get
    }
    val currentTarget = unpacker.unpackLong()
    val nonce = unpacker.unpackLong()
    val timestamp = unpacker.unpackLong()
    val version = unpacker.unpackByte()
    BDBlock(txs, parentId, currentTarget, nonce, version, timestamp)
  }

}

