package transaction

import scorex.core.serialization.ScorexSerializer
import scorex.core.transaction.Transaction
import scorex.core.transaction.proof.{Signature25519, Signature25519Serializer}
import scorex.util.ByteArrayBuilder
import scorex.util.serialization.{Reader, VLQByteBufferWriter, Writer}

import scala.util.Try

case class BDTransaction(inputs: IndexedSeq[OutputId],
                         outputs: IndexedSeq[BDOutput],
                         signatures: IndexedSeq[Signature25519]
                        ) extends Transaction {

  override val messageToSign: Array[Byte] = {
    val writer = new VLQByteBufferWriter(new ByteArrayBuilder())
    BDTransactionSerializer.serializeNoSignatures(this, writer)
    writer.result().toBytes
  }

}

object BDTransactionSerializer extends ScorexSerializer[BDTransaction] {

  def serializeNoSignatures(obj: BDTransaction, w: Writer): Unit = {
    w.putInt(obj.inputs.size)
    obj.inputs.foreach{ i =>
      w.putBytes(i)
    }
    w.putInt(obj.outputs.size)
    obj.outputs.foreach{ o =>
      BDOutputSerializer.serialize(o, w)
    }

  }

  override def serialize(obj: BDTransaction, w: Writer): Unit = {
    serializeNoSignatures(obj, w)

    w.putInt(obj.signatures.size)
    obj.signatures.foreach{ s =>
      Signature25519Serializer.serialize(s, w)
    }
  }

  override def parse(r: Reader): BDTransaction = {
    val inputsSize = r.getInt()
    val inputs = (0 until inputsSize) map { _ =>
      OutputId @@ r.getBytes(32)
    }

    val outputsSize = r.getInt()
    val outputs = (0 until outputsSize) map { _ =>
      BDOutputSerializer.parse(r)
    }

    val sigSize = r.getInt()
    val signatures = (0 until sigSize) map { _ =>
      Signature25519Serializer.parse(r)
    }

    BDTransaction(inputs, outputs, signatures)

  }

}