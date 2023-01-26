interface ConsistentHash<K> {
    fun getShardByKey(key: K): Shard

    fun addShard(newShard: Shard, vnodeHashes: Set<Int>): Map<Shard, Set<HashRange>>

    fun removeShard(shard: Shard): Map<Shard, Set<HashRange>>
}