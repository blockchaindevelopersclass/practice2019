package nodeViewHolder

import scorex.core.transaction.MemoryPool
import scorex.util.ModifierId
import transaction.BDTransaction

import scala.util.Try

case class BDMempool(poolTxs: Seq[BDTransaction] = Seq())
  extends MemoryPool[BDTransaction, BDMempool] {

  override def modifierById(id: ModifierId): Option[BDTransaction] = poolTxs.find(_.id == id)

  override def put(tx: BDTransaction): Try[BDMempool] = put(Seq(tx))

  override def put(txs: Iterable[BDTransaction]): Try[BDMempool] = Try {
    // todo some checks here
    putWithoutCheck(txs)
  }

  override def putWithoutCheck(txs: Iterable[BDTransaction]): BDMempool = {
    val unique = txs.map(tx => tx.id -> tx).toMap.values
    val newTransactions = unique.filter(tx => !poolTxs.contains(tx)).take(BDMempool.Limit - poolTxs.size)
    new BDMempool(poolTxs ++ newTransactions)
  }

  override def remove(tx: BDTransaction): BDMempool = {
    new BDMempool(poolTxs.filter(poolTx => poolTx != tx))
  }

  override def filter(condition: BDTransaction => Boolean): BDMempool = {
    new BDMempool(poolTxs.filter(condition))
  }

  override def contains(id: ModifierId): Boolean = poolTxs.exists(_.id == id)

  override def getAll(ids: Seq[ModifierId]): Seq[BDTransaction] = poolTxs.filter(tx => ids.contains(tx.id))

  override def size: Int = poolTxs.size

  override def take(limit: Int): Seq[BDTransaction] = poolTxs.take(limit)

  override type NVCT = BDMempool
}

object BDMempool {
  val Limit = 500

  val empty: BDMempool = BDMempool(Seq.empty)
}