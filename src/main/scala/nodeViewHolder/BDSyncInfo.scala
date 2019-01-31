package nodeViewHolder

import blocks.BDBlock
import scorex.util.ModifierId
import scorex.core.consensus.History.ModifierIds
import scorex.core.consensus.SyncInfo
import scorex.core.{bytesToId, idToBytes}
import scorex.core.network.message.SyncInfoMessageSpec
import scorex.core.serialization.ScorexSerializer
import scorex.util.serialization.{Reader, Writer}

import scala.util.Try

case class BDSyncInfo(ids: Seq[ModifierId]) extends SyncInfo {

  override val startingPoints: ModifierIds = Seq((BDBlock.BDBlockModifierTypeId, ids.head))

}

object BDSyncInfoSerializer extends ScorexSerializer[BDSyncInfo] {

  override def serialize(obj: BDSyncInfo, w: Writer): Unit = {
    w.putInt(obj.ids.size)
    obj.ids.foreach(id => w.putBytes(idToBytes(id)))
  }

  override def parse(r: Reader): BDSyncInfo = {
    val ids = (0 until r.getInt()) map { _ =>
      bytesToId(r.getBytes(32))
    }
    BDSyncInfo(ids)
  }
}


object BDSyncInfo {
  val idsSize = 100
}

object BDSyncInfoMessageSpec extends SyncInfoMessageSpec[BDSyncInfo](BDSyncInfoSerializer)