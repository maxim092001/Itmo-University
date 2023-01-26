import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions.*
import kotlin.random.Random

class ConsistentHashImplUnitTest {
    @Test
    fun testAddSingleShard() {
        val random = Random(System.currentTimeMillis())
        val cHash = ConsistentHashImpl<Int>()
        val shard1 = Shard(shardName = "shard_1")
        val addRes = cHash.addShard(newShard = shard1, vnodeHashes = setOf(100))
        assertEquals(addRes, emptyMap<Shard, Set<HashRange>>())
        repeat(100) {
            val key = random.nextInt()
            assertEquals(cHash.getShardByKey(key), shard1)
        }
    }

    @Test
    fun testAddMultipleVnodesSingleShard() {
        val random = Random(System.currentTimeMillis())
        val cHash = ConsistentHashImpl<Int>()
        val shard1 = Shard(shardName = "shard_1")
        val addRes = cHash.addShard(
            newShard = shard1,
            vnodeHashes = setOf(-100_000, 100_000, 300_000, 500_000, 900_000)
        )
        assertEquals(addRes, emptyMap<Shard, Set<HashRange>>())
        repeat(100) {
            val key = random.nextInt()
            assertEquals(cHash.getShardByKey(key), shard1)
        }
    }

    @Test
    fun testAddMultipleShardsSingleVnode() {
        val cHash = ConsistentHashImpl<Int>()

        val shard1 = Shard(shardName = "shard_1")
        var addRes = cHash.addShard(newShard = shard1, vnodeHashes = setOf(100))
        assertEquals(addRes, emptyMap<Shard, Set<HashRange>>())

        val shard2 = Shard(shardName = "shard_2")
        addRes = cHash.addShard(newShard = shard2, vnodeHashes = setOf(200))
        assertEquals(
            addRes,
            mapOf(
                Pair(shard1, setOf(HashRange(leftBorder = 101, rightBorder = 200)))
            )
        )

        val shard3 = Shard(shardName = "shard_3")
        addRes = cHash.addShard(newShard = shard3, vnodeHashes = setOf(150))
        assertEquals(
            addRes,
            mapOf(
                Pair(shard2, setOf(HashRange(leftBorder = 101, rightBorder = 150)))
            )
        )

        val shard4 = Shard(shardName = "shard_4")
        addRes = cHash.addShard(newShard = shard4, vnodeHashes = setOf(50))
        assertEquals(
            addRes,
            mapOf(
                Pair(shard1, setOf(HashRange(leftBorder = 201, rightBorder = 50)))
            )
        )

        val shard5 = Shard(shardName = "shard_5")
        addRes = cHash.addShard(newShard = shard5, vnodeHashes = setOf(500))
        assertEquals(
            addRes,
            mapOf(
                Pair(shard4, setOf(HashRange(leftBorder = 201, rightBorder = 500)))
            )
        )

        assertEquals(cHash.getShardByKey(-100), shard4)
        assertEquals(cHash.getShardByKey(10), shard4)
        assertEquals(cHash.getShardByKey(75), shard1)
        assertEquals(cHash.getShardByKey(100), shard1)
        assertEquals(cHash.getShardByKey(300), shard5)
        assertEquals(cHash.getShardByKey(1_000_000), shard4)
    }

    @Test
    fun testOneAfterAnother() {
        val cHash = ConsistentHashImpl<Int>()

        val shard1 = Shard(shardName = "shard_1")
        var addRes = cHash.addShard(newShard = shard1, vnodeHashes = setOf(100, 1000, 2000))
        assertEquals(addRes, emptyMap<Shard, Set<HashRange>>())

        val shard2 = Shard(shardName = "shard_2")
        addRes = cHash.addShard(newShard = shard2, vnodeHashes = setOf(200, 500, 400))
        assertEquals(
            addRes,
            mapOf(
                Pair(shard1, setOf(HashRange(leftBorder = 101, rightBorder = 500)))
            )
        )

        assertEquals(cHash.getShardByKey(100), shard1)
        assertEquals(cHash.getShardByKey(-100), shard1)
        assertEquals(cHash.getShardByKey(150), shard2)
        assertEquals(cHash.getShardByKey(500), shard2)
        assertEquals(cHash.getShardByKey(501), shard1)
        assertEquals(cHash.getShardByKey(3000), shard1)
    }

    @Test
    fun testOneAfterAnotherCircleEnd() {
        val cHash = ConsistentHashImpl<Int>()

        val shard1 = Shard(shardName = "shard_1")
        var addRes = cHash.addShard(newShard = shard1, vnodeHashes = setOf(100, 1000, 2000))
        assertEquals(addRes, emptyMap<Shard, Set<HashRange>>())

        val shard2 = Shard(shardName = "shard_2")
        addRes = cHash.addShard(newShard = shard2, vnodeHashes = setOf(3000, -100, -500, 2500))
        assertEquals(
            addRes,
            mapOf(
                Pair(shard1, setOf(HashRange(leftBorder = 2001, rightBorder = -100)))
            )
        )
    }

    @Test
    fun testMultipleRangesReplaceSameShard() {
        val cHash = ConsistentHashImpl<Int>()

        val shard1 = Shard(shardName = "shard_1")
        var addRes = cHash.addShard(newShard = shard1, vnodeHashes = setOf(300, 1200, 2200))
        assertEquals(addRes, emptyMap<Shard, Set<HashRange>>())

        val shard2 = Shard(shardName = "shard_2")
        addRes = cHash.addShard(newShard = shard2, vnodeHashes = setOf(1700, 4200, 3200))
        assertEquals(
            addRes,
            mapOf(
                Pair(
                    shard1,
                    setOf(
                        HashRange(leftBorder = 1201, rightBorder = 1700),
                        HashRange(leftBorder = 2201, rightBorder = 4200)
                    )
                )
            )
        )

        assertEquals(cHash.getShardByKey(4201), shard1)
    }

    @Test
    fun testMultipleRangesReplaceMultipleShard() {
        val cHash = ConsistentHashImpl<Int>()

        val shard1 = Shard(shardName = "shard_1")
        var addRes = cHash.addShard(newShard = shard1, vnodeHashes = setOf(100, 1000, 2000))
        assertEquals(addRes, emptyMap<Shard, Set<HashRange>>())

        val shard2 = Shard(shardName = "shard_2")
        addRes = cHash.addShard(newShard = shard2, vnodeHashes = setOf(1500, 4000, 3000))
        assertEquals(
            addRes,
            mapOf(
                Pair(
                    shard1,
                    setOf(
                        HashRange(leftBorder = 2001, rightBorder = 4000),
                        HashRange(leftBorder = 1001, rightBorder = 1500)
                    )
                )
            )
        )

        val shard3 = Shard(shardName = "shard_3")
        addRes = cHash.addShard(newShard = shard3, vnodeHashes = setOf(5000, -100, -200, 1300, 1250))
        assertEquals(
            addRes,
            mapOf(
                Pair(shard2, setOf(HashRange(leftBorder = 1001, rightBorder = 1300))),
                Pair(shard1, setOf(HashRange(leftBorder = 4001, rightBorder = -100)))
            )
        )
    }

    @Test
    fun testAddMultipleShardsMultipleVnodesStress() {
        val cHash = ConsistentHashImpl<Int>()

        val shard1 = Shard(shardName = "shard_1")
        var addRes = cHash.addShard(newShard = shard1, vnodeHashes = setOf(100, 1000, 2000))
        assertEquals(addRes, emptyMap<Shard, Set<HashRange>>())

        val shard2 = Shard(shardName = "shard_2")
        addRes = cHash.addShard(newShard = shard2, vnodeHashes = setOf(200, 3000, -100))
        assertEquals(
            addRes,
            mapOf(
                Pair(
                    shard1,
                    setOf(
                        HashRange(leftBorder = 2001, rightBorder = -100),
                        HashRange(leftBorder = 101, rightBorder = 200)
                    )
                )
            )
        )

        val shard3 = Shard(shardName = "shard_3")
        addRes = cHash.addShard(newShard = shard3, vnodeHashes = setOf(300, -200, 400))
        assertEquals(
            addRes,
            mapOf(
                Pair(shard1, setOf(HashRange(leftBorder = 201, rightBorder = 400))),
                Pair(shard2, setOf(HashRange(leftBorder = 3001, rightBorder = -200)))
            )
        )

        val shard4 = Shard(shardName = "shard_4")
        addRes = cHash.addShard(newShard = shard4, vnodeHashes = setOf(1500, 1800, 1700))
        assertEquals(
            addRes,
            mapOf(
                Pair(shard1, setOf(HashRange(leftBorder = 1001, rightBorder = 1800)))
            )
        )

        val shard5 = Shard(shardName = "shard_5")
        addRes = cHash.addShard(newShard = shard5, vnodeHashes = setOf(1600, 1750, 150))
        assertEquals(
            addRes,
            mapOf(
                Pair(shard2, setOf(HashRange(leftBorder = 101, rightBorder = 150))),
                Pair(
                    shard4,
                    setOf(
                        HashRange(leftBorder = 1701, rightBorder = 1750),
                        HashRange(leftBorder = 1501, rightBorder = 1600)
                    )
                )
            )
        )

        val shard6 = Shard(shardName = "shard_6")
        addRes = cHash.addShard(newShard = shard6, vnodeHashes = setOf(4000, -300))
        assertEquals(
            addRes,
            mapOf(
                Pair(shard3, setOf(HashRange(leftBorder = 3001, rightBorder = -300))),
            )
        )

        assertEquals(cHash.getShardByKey(-100), shard2)
        assertEquals(cHash.getShardByKey(1602), shard4)
        assertEquals(cHash.getShardByKey(-150), shard2)
        assertEquals(cHash.getShardByKey(350), shard3)
    }

    @Test
    fun testAddAndRemoveSingleShard() {
        val cHash = ConsistentHashImpl<Int>()

        val shard1 = Shard(shardName = "shard_1")
        var addRes = cHash.addShard(newShard = shard1, vnodeHashes = setOf(100, 1000, 2000))
        assertEquals(addRes, emptyMap<Shard, Set<HashRange>>())

        val shard2 = Shard(shardName = "shard_2")
        addRes = cHash.addShard(newShard = shard2, vnodeHashes = setOf(1500))
        assertEquals(
            addRes,
            mapOf(
                Pair(shard1, setOf(HashRange(leftBorder = 1001, rightBorder = 1500)))
            )
        )

        val removeRes = cHash.removeShard(shard = shard2)
        assertEquals(
            removeRes,
            mapOf(
                Pair(shard1, setOf(HashRange(leftBorder = 1001, rightBorder = 1500)))
            )
        )

        val random = Random(System.currentTimeMillis())
        repeat(100) {
            val key = random.nextInt()
            assertEquals(cHash.getShardByKey(key), shard1)
        }
    }

    @Test
    fun testAddNewAndRemoveOldShard() {
        val cHash = ConsistentHashImpl<Int>()

        val shard1 = Shard(shardName = "shard_1")
        var addRes = cHash.addShard(newShard = shard1, vnodeHashes = setOf(200, 1100, 2100))
        assertEquals(addRes, emptyMap<Shard, Set<HashRange>>())

        val shard2 = Shard(shardName = "shard_2")
        addRes = cHash.addShard(newShard = shard2, vnodeHashes = setOf(1600))
        assertEquals(
            addRes,
            mapOf(
                Pair(shard1, setOf(HashRange(leftBorder = 1101, rightBorder = 1600)))
            )
        )

        val removeRes = cHash.removeShard(shard = shard1)
        assertEquals(
            removeRes,
            mapOf(
                Pair(shard2, setOf(HashRange(leftBorder = 1601, rightBorder = 1100)))
            )
        )

        val random = Random(System.currentTimeMillis())
        repeat(100) {
            val key = random.nextInt()
            assertEquals(cHash.getShardByKey(key), shard2)
        }
    }

    @Test
    fun testRemoveMultipleRangesSameShard() {
        val cHash = ConsistentHashImpl<Int>()

        val shard1 = Shard(shardName = "shard_1")
        var addRes = cHash.addShard(newShard = shard1, vnodeHashes = setOf(100, 500, 1000))
        assertEquals(addRes, emptyMap<Shard, Set<HashRange>>())

        val shard2 = Shard(shardName = "shard_2")
        addRes = cHash.addShard(newShard = shard2, vnodeHashes = setOf(2000, 300))
        assertEquals(
            addRes,
            mapOf(
                Pair(
                    shard1,
                    setOf(
                        HashRange(leftBorder = 1001, rightBorder = 2000),
                        HashRange(leftBorder = 101, rightBorder = 300)
                    )
                )
            )
        )

        val shard3 = Shard(shardName = "shard_3")
        addRes = cHash.addShard(newShard = shard3, vnodeHashes = setOf(250, 200, 1500, 1700))
        assertEquals(
            addRes,
            mapOf(
                Pair(
                    shard2,
                    setOf(
                        HashRange(leftBorder = 1001, rightBorder = 1700),
                        HashRange(leftBorder = 101, rightBorder = 250)
                    )
                )
            )
        )

        val removeRes = cHash.removeShard(shard = shard3)
        assertEquals(
            removeRes,
            mapOf(
                Pair(
                    shard2,
                    setOf(
                        HashRange(leftBorder = 1001, rightBorder = 1700),
                        HashRange(leftBorder = 101, rightBorder = 250)
                    )
                )
            )
        )
    }

    @Test
    fun testRemoveMultipleRangesSameShardRangeBeginEnd() {
        val cHash = ConsistentHashImpl<Int>()

        val shard1 = Shard(shardName = "shard_1")
        var addRes = cHash.addShard(newShard = shard1, vnodeHashes = setOf(200, 1000, 2000))
        assertEquals(addRes, emptyMap<Shard, Set<HashRange>>())

        val shard2 = Shard(shardName = "shard_2")
        addRes = cHash.addShard(newShard = shard2, vnodeHashes = setOf(3000, 1500, 500))
        assertEquals(
            addRes,
            mapOf(
                Pair(
                    shard1,
                    setOf(
                        HashRange(leftBorder = 201, rightBorder = 500),
                        HashRange(leftBorder = 1001, rightBorder = 1500),
                        HashRange(leftBorder = 2001, rightBorder = 3000),
                    )
                )
            )
        )

        val shard3 = Shard(shardName = "shard_3")
        addRes = cHash.addShard(newShard = shard3, vnodeHashes = setOf(4000, -100, 700))
        assertEquals(
            addRes,
            mapOf(
                Pair(
                    shard1,
                    setOf(
                        HashRange(leftBorder = 501, rightBorder = 700),
                        HashRange(leftBorder = 3001, rightBorder = -100)
                    )
                )
            )
        )

        val removeRes = cHash.removeShard(shard = shard3)
        assertEquals(
            removeRes,
            mapOf(
                Pair(
                    shard1,
                    setOf(
                        HashRange(leftBorder = 501, rightBorder = 700),
                        HashRange(leftBorder = 3001, rightBorder = -100)
                    )
                )
            )
        )
    }

    @Test
    fun testRemoveMultipleRangesMultipleShards() {
        val cHash = ConsistentHashImpl<Int>()

        val shard1 = Shard(shardName = "shard_1")
        var addRes = cHash.addShard(newShard = shard1, vnodeHashes = setOf(200, 1000, 2000))
        assertEquals(addRes, emptyMap<Shard, Set<HashRange>>())

        val shard2 = Shard(shardName = "shard_2")
        addRes = cHash.addShard(newShard = shard2, vnodeHashes = setOf(3000, 4000))
        assertEquals(
            addRes,
            mapOf(
                Pair(
                    shard1,
                    setOf(
                        HashRange(leftBorder = 2001, rightBorder = 4000)
                    )
                )
            )
        )

        val shard3 = Shard(shardName = "shard_3")
        addRes = cHash.addShard(newShard = shard3, vnodeHashes = setOf(5000, -100, 700, 500, 2500))
        assertEquals(
            addRes,
            mapOf(
                Pair(
                    shard1,
                    setOf(
                        HashRange(leftBorder = 4001, rightBorder = -100),
                        HashRange(leftBorder = 201, rightBorder = 700)
                    )
                ),
                Pair(shard2, setOf(HashRange(leftBorder = 2001, rightBorder = 2500)))
            )
        )

        val removeRes = cHash.removeShard(shard = shard3)
        assertEquals(
            removeRes,
            mapOf(
                Pair(
                    shard1,
                    setOf(
                        HashRange(leftBorder = 4001, rightBorder = -100),
                        HashRange(leftBorder = 201, rightBorder = 700)
                    )
                ),
                Pair(shard2, setOf(HashRange(leftBorder = 2001, rightBorder = 2500)))
            )
        )
    }

    @Test
    fun testRemoveOldShardMultipleRangesMultipleShards() {
        val cHash = ConsistentHashImpl<Int>()

        val shard1 = Shard(shardName = "shard_1")
        var addRes = cHash.addShard(newShard = shard1, vnodeHashes = setOf(200, 1000, 2000))
        assertEquals(addRes, emptyMap<Shard, Set<HashRange>>())

        val shard2 = Shard(shardName = "shard_2")
        addRes = cHash.addShard(newShard = shard2, vnodeHashes = setOf(3000, 4000))
        assertEquals(
            addRes,
            mapOf(
                Pair(
                    shard1,
                    setOf(
                        HashRange(leftBorder = 2001, rightBorder = 4000)
                    )
                )
            )
        )

        val shard3 = Shard(shardName = "shard_3")
        addRes = cHash.addShard(newShard = shard3, vnodeHashes = setOf(5000, -100, 700, 500, 2500))
        assertEquals(
            addRes,
            mapOf(
                Pair(
                    shard1,
                    setOf(
                        HashRange(leftBorder = 4001, rightBorder = -100),
                        HashRange(leftBorder = 201, rightBorder = 700)
                    )
                ),
                Pair(shard2, setOf(HashRange(leftBorder = 2001, rightBorder = 2500)))
            )
        )

        val removeRes = cHash.removeShard(shard = shard2)
        assertEquals(
            removeRes,
            mapOf(
                Pair(shard3, setOf(HashRange(leftBorder = 2501, rightBorder = 4000)))
            )
        )
    }

    @Test
    fun testRemoveOldestShardMultipleRangesMultipleShards() {
        val cHash = ConsistentHashImpl<Int>()

        val shard1 = Shard(shardName = "shard_1")
        var addRes = cHash.addShard(newShard = shard1, vnodeHashes = setOf(200, 1000, 2000))
        assertEquals(addRes, emptyMap<Shard, Set<HashRange>>())

        val shard2 = Shard(shardName = "shard_2")
        addRes = cHash.addShard(newShard = shard2, vnodeHashes = setOf(3000, 4000))
        assertEquals(
            addRes,
            mapOf(
                Pair(
                    shard1,
                    setOf(
                        HashRange(leftBorder = 2001, rightBorder = 4000)
                    )
                )
            )
        )

        val shard3 = Shard(shardName = "shard_3")
        addRes = cHash.addShard(newShard = shard3, vnodeHashes = setOf(5000, -100, 700, 500, 2500))
        assertEquals(
            addRes,
            mapOf(
                Pair(
                    shard1,
                    setOf(
                        HashRange(leftBorder = 4001, rightBorder = -100),
                        HashRange(leftBorder = 201, rightBorder = 700)
                    )
                ),
                Pair(shard2, setOf(HashRange(leftBorder = 2001, rightBorder = 2500)))
            )
        )

        val removeRes = cHash.removeShard(shard = shard1)
        assertEquals(
            removeRes,
            mapOf(
                Pair(
                    shard3,
                    setOf(
                        HashRange(leftBorder = 701, rightBorder = 2000),
                        HashRange(leftBorder = -99, rightBorder = 200)
                    )
                )
            )
        )
    }

    @Test
    fun testExpandingEndBeginPrevious() {
        val cHash = ConsistentHashImpl<Int>()

        val shard1 = Shard(shardName = "shard_1")
        var addRes = cHash.addShard(newShard = shard1, vnodeHashes = setOf(1000))
        assertEquals(addRes, emptyMap<Shard, Set<HashRange>>())

        val shard2 = Shard(shardName = "shard_2")
        addRes = cHash.addShard(newShard = shard2, vnodeHashes = setOf(2000, 100))
        assertEquals(
                addRes,
                mapOf(
                        Pair(
                                shard1,
                                setOf(
                                        HashRange(leftBorder = 1001, rightBorder = 100)
                                )
                        )
                )
        )
    }

    @Test
    fun testExpandingBeginEndPrevious() {
        val cHash = ConsistentHashImpl<Int>()

        val shard1 = Shard(shardName = "shard_1")
        var addRes = cHash.addShard(newShard = shard1, vnodeHashes = setOf(1000))
        assertEquals(addRes, emptyMap<Shard, Set<HashRange>>())

        val shard2 = Shard(shardName = "shard_2")
        addRes = cHash.addShard(newShard = shard2, vnodeHashes = setOf(2000, 3000))
        assertEquals(
                addRes,
                mapOf(
                        Pair(
                                shard1,
                                setOf(
                                        HashRange(leftBorder = 1001, rightBorder = 3000)
                                )
                        )
                )
        )
    }

    @Test
    fun testExpandingPreviousEndBegin() {
        val cHash = ConsistentHashImpl<Int>()

        val shard1 = Shard(shardName = "shard_1")
        var addRes = cHash.addShard(newShard = shard1, vnodeHashes = setOf(1000))
        assertEquals(addRes, emptyMap<Shard, Set<HashRange>>())

        val shard2 = Shard(shardName = "shard_2")
        addRes = cHash.addShard(newShard = shard2, vnodeHashes = setOf(100, 200))
        assertEquals(
                addRes,
                mapOf(
                        Pair(
                                shard1,
                                setOf(
                                        HashRange(leftBorder = 1001, rightBorder = 200)
                                )
                        )
                )
        )
    }
}