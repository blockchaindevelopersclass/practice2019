package transaction

import scorex.core.serialization.Serializer
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Sha256

import scala.util.Try

case class Output(proposition: PublicKey25519Proposition, value: Value) extends Box[PublicKey25519Proposition] {
  override val id: ADKey = ADKey !@@ Sha256(serializer.toBytes(this))
  override type M = Output

  override def serializer: Serializer[Output] = OutputSerializer
}


object OutputSerializer extends Serializer[Output] {
  override def toBytes(obj: Output): Array[Byte] = ???

  override def parseBytes(bytes: Array[Byte]): Try[Output] = ???
}