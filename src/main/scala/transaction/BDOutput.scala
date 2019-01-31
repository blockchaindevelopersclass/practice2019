package transaction

import scorex.core.serialization.ScorexSerializer
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.{PublicKey25519Proposition, PublicKey25519PropositionSerializer}
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Blake2b256
import scorex.util.serialization.{Reader, Writer}

case class BDOutput(proposition: PublicKey25519Proposition, value: Value) extends Box[PublicKey25519Proposition] {
  override val id: ADKey = ADKey !@@ Blake2b256(BDOutputSerializer.toBytes(this))

  override def equals(obj: Any): Boolean = obj match {
    case that: BDOutput => id sameElements that.id
    case _ => false
  }
}


object BDOutputSerializer extends ScorexSerializer[BDOutput] {

  override def serialize(obj: BDOutput, w: Writer): Unit = {
    PublicKey25519PropositionSerializer.serialize(obj.proposition, w)
    w.putULong(obj.value)
  }

  override def parse(r: Reader): BDOutput = {
    BDOutput(PublicKey25519PropositionSerializer.parse(r), Value @@ r.getULong())
  }
}