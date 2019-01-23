package transaction

import com.google.common.primitives.Bytes
import org.msgpack.core.MessagePack
import scorex.core.serialization.Serializer
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.core.utils.concatBytes
import scorex.crypto.signatures.{PublicKey, Signature}
import supertagged.untag

import scala.util.Try

case class BDTransaction(inputs: IndexedSeq[OutputId],
                         outputs: IndexedSeq[Output],
                         signatures: IndexedSeq[Signature25519]
                        ) extends Transaction {
  override type M = BDTransaction

  private def seqToBytes[A](sequence: IndexedSeq[A], mapping: A => Array[Byte]): Array[Byte] =
    if (sequence.nonEmpty) concatBytes(sequence.map(mapping)) else Array[Byte]()

  override val messageToSign: Array[Byte] = Bytes.concat(
    seqToBytes[OutputId](
      inputs,
      i => untag(i)),
    seqToBytes[Output](
      outputs,
      o => o.serializer.toBytes(o)),
    seqToBytes[Signature25519](
      signatures,
      s => s.serializer.toBytes(s))
  )

  override def serializer: Serializer[BDTransaction] = BDTransactionSerializer

}

object BDTransactionSerializer extends Serializer[BDTransaction] {
  override def toBytes(obj: BDTransaction): Array[Byte] = {
    val packer = MessagePack.newDefaultBufferPacker()
    packer.packArrayHeader(obj.inputs.size)
    for {
      input <- obj.inputs
    } yield {
      packer.packBinaryHeader(input.length)
      packer.writePayload(input)
    }
    packer.packArrayHeader(obj.outputs.size)
    for {
      output <- obj.outputs
    } yield {
      packer.packBinaryHeader(output.proposition.bytes.length)
      packer.writePayload(output.proposition.bytes)
      packer.packLong(output.value)
    }
    packer.packArrayHeader(obj.signatures.size)
    for {
      signature <- obj.signatures
    } yield {
      packer.packBinaryHeader(signature.signature.length)
      packer.writePayload(signature.signature)
    }
    packer.toByteArray
  }

  override def parseBytes(bytes: Array[Byte]): Try[BDTransaction] = Try {
    val unpacker = MessagePack.newDefaultUnpacker(bytes)
    val numInputs = unpacker.unpackArrayHeader()
    val inputs = for {
      i <- Range(0, numInputs)
    } yield {
      val binaryLen = unpacker.unpackBinaryHeader()
      OutputId @@ unpacker.readPayload(binaryLen)
    }
    val numOutputs = unpacker.unpackArrayHeader()
    val outputs = for {
      i <- Range(0, numOutputs)
    } yield {
      val binaryLen = unpacker.unpackBinaryHeader()
      Output(
        PublicKey25519Proposition(PublicKey @@ unpacker.readPayload(binaryLen)),
        Value @@ unpacker.unpackLong()
      )
    }
    val numTransactions = unpacker.unpackArrayHeader()
    val transactions = for {
      i <- Range(0, numTransactions)
    } yield {
      val binaryLen = unpacker.unpackBinaryHeader()
      Signature25519(Signature @@ unpacker.readPayload(binaryLen))
    }
    BDTransaction(inputs, outputs, transactions)
  }
}