package serialization

import blocks.BDBlockSerializer
import nodeViewHolder.BDSyncInfoSerializer
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import transaction.{BDOutputSerializer, BDTransactionSerializer}
import utils.Generators

class SerializationTests extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with Generators {

  property("BDSyncInfoGen should be serialized and deserialized") {
    val serializer = BDSyncInfoSerializer
    forAll(BDSyncInfoGen) { obj =>
      val recovered = serializer.parseBytes(serializer.toBytes(obj))
      serializer.toBytes(obj) shouldEqual serializer.toBytes(recovered)
    }
  }

  property("BDBlock should be serialized and deserialized") {
    val serializer = BDBlockSerializer
    forAll(BDBlockGenerator) { obj =>
      val recovered = serializer.parseBytes(serializer.toBytes(obj))
      serializer.toBytes(obj) shouldEqual serializer.toBytes(recovered)
    }
  }

  property("BDTransactions should be serialized and deserialized") {
    val serializer = BDTransactionSerializer
    forAll(BDTransactionGenerator) { obj =>
      val recovered = serializer.parseBytes(serializer.toBytes(obj))
      serializer.toBytes(obj) shouldEqual serializer.toBytes(recovered)
    }
  }

  property("BDOutput should be serialized and deserialized") {
    val serializer = BDOutputSerializer
    forAll(outputGen) { obj =>
      val recovered = serializer.parseBytes(serializer.toBytes(obj))
      serializer.toBytes(obj) shouldEqual serializer.toBytes(recovered)
    }
  }

}