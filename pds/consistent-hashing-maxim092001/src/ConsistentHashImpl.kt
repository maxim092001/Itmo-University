typealias Hash = Int

class ConsistentHashImpl<K> : ConsistentHash<K> {

    private val ring: ArrayList<ShardAndHash> = ArrayList()
    private var shards: Int = 0

    override fun getShardByKey(key: K): Shard {
        val hash = key.hashCode()
        val binPos = ring.binarySearch(ShardAndHash(null, hash), comparator = compareBy { it.hash })
        val nonBinPos = if (binPos >= 0) binPos else -binPos - 1
        val realPos = if (nonBinPos == ring.size) 0 else nonBinPos
        return ring[realPos].shard!!
    }

    override fun addShard(newShard: Shard, vnodeHashes: Set<Int>): Map<Shard, Set<HashRange>> {
        val map: MutableMap<Shard, HashSet<HashRange>> = HashMap()
        val nodes = ArrayList(vnodeHashes).sorted()
        if (shards == 0) {
            for (hash in nodes) {
                ring.add(ShardAndHash(newShard, hash))
            }
            shards++
        } else {
            var idx = 0
            var lastHash = if (ring[0].hash == 0) 0 else ring[ring.size - 1].hash
            var pos = 0
            var lastPos: Int? = null
            var st: Int

            if (ring.last().hash < nodes.last()) {
                st = ring.last().hash + 1

                lastPos =  nodes.indexOfLast { it <= ring.last().hash }

                while (nodes[idx] < ring[pos].hash) {
                    ring.add(pos, ShardAndHash(newShard, nodes[idx]))
                    idx++
                    pos++
                }

                val set = getRanges(map, pos)

                if (pos > 0) {
                    set.add(HashRange(st, ring[pos - 1].hash))
                    lastHash = ring.first().hash
                } else {
                    set.add(HashRange(st, nodes[nodes.size - 1]))
                }

            }

            while (pos < ring.size) {
                if (nodes[idx] < ring[pos].hash) {
                    st = lastHash + 1

                    val set = getRanges(map, pos)
                    while (nodes.size > idx && nodes[idx] < ring[pos].hash) {
                        ring.add(pos, ShardAndHash(newShard, nodes[idx]))
                        pos++
                        idx++
                    }
                    set.add(HashRange(st, ring[pos - 1].hash))
                    if (nodes.size <= idx) break

                } else {
                    lastHash = ring[pos].hash
                    pos++
                }
            }

            if (lastPos != null) {
                (lastPos + 1 until nodes.size).forEach {
                    ring.add(ring.size, ShardAndHash(newShard, nodes[it]))
                }
            }

            map.remove(newShard)
            shards++

            cleanMap(map)
        }
        return map
    }

    override fun removeShard(shard: Shard): Map<Shard, Set<HashRange>> {
        val map: MutableMap<Shard, HashSet<HashRange>> = HashMap()
        if (shards == 1) {
            ring.clear()
        } else {
            val (position, ranges, hashRangeEnd) = getPosAndRanges(shard, map)
            if (hashRangeEnd != null) ranges.add(HashRange(ring[position].hash + 1, hashRangeEnd))
            shards--
            cleanMap(map)
        }
        return map
    }

    private fun getPosAndRanges(shard: Shard, map: MutableMap<Shard, HashSet<HashRange>>): Triple<Int, HashSet<HashRange>, Int?> {
        var (position, ranges, startHash) = startInfo(shard, map)
        var hashRangeEnd: Int? = null

        while (ring[position].hash != startHash) {
            if (ring[position].shard == shard) {
                if (hashRangeEnd == null) hashRangeEnd = ring[position].hash
                ring.remove(ring[position])
            } else {
                if (hashRangeEnd != null) {
                    ranges.add(HashRange(ring[position].hash + 1, hashRangeEnd))
                    hashRangeEnd = null
                }
                ranges = getRanges(map, position)
            }
            if (position-- == 0) position = ring.lastIndex
        }

        return Triple(position, ranges, hashRangeEnd)
    }

    private fun startInfo(
        shard: Shard,
        map: MutableMap<Shard, HashSet<HashRange>>
    ): Triple<Int, HashSet<HashRange>, Hash> {
        val startPos = ring.indexOfFirst { it.shard != shard }

        return Triple(
            if (startPos == 0) ring.lastIndex else startPos - 1,
            getRanges(map, startPos),
            ring[startPos].hash
        )
    }

    private fun cleanMap(mp: MutableMap<Shard, HashSet<HashRange>>) =
        mp.entries.filter { it.value.size == 0 }.map { it.key }.forEach { x -> mp.remove(x) }


    private fun getRanges(mp: MutableMap<Shard, HashSet<HashRange>>, ringIdx: Int): HashSet<HashRange> {
        val rng = if (ring[ringIdx].shard == null) null else mp[ring[ringIdx].shard]
        fun emptySet(): HashSet<HashRange> {
            val empty = HashSet<HashRange>()
            mp[ring[ringIdx].shard!!] = empty
            return empty
        }
        return rng ?: emptySet()
    }

    companion object {
        private data class ShardAndHash(val shard: Shard?, val hash: Hash)
    }
}