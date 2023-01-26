import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import system.DataHolderEnvironment
import system.DataHolderEnvironmentImpl
import system.MergerEnvironmentImpl
import kotlin.random.Random

class SolutionTest {
    @Test
    fun simpleNoFailuresTest() {
        val dataHolderEnvironment = DataHolderEnvironmentImpl(batchSize = 3)
        val dataHolders = listOf(
            DataHolderImpl(
                dataHolderEnvironment = dataHolderEnvironment,
                keys = listOf(1, 3, 5, 7, 9, 11, 13, 15)
            ),
            DataHolderImpl(
                dataHolderEnvironment = dataHolderEnvironment,
                keys = listOf(2, 4, 6, 8, 10)
            )
        )
        val mergerEnvironment = MergerEnvironmentImpl(dataHolders, dataHolderEnvironment)

        mergerEnvironment.newMergerStarted(null)
        val firstMerger = MergerImpl(mergerEnvironment, null)
        for (i in 1..7) {
            assertEquals(i, firstMerger.mergeStep())
            mergerEnvironment.keyYielded()
        }

        val remaining = firstMerger.getRemainingBatches()
        assertEquals(
            mapOf(
                Pair(0, listOf(9, 11)),
                Pair(1, listOf(8, 10))
            ),
            remaining
        )
        mergerEnvironment.newMergerStarted(remaining)
        val secondMerger = MergerImpl(mergerEnvironment, remaining)
        for (i in listOf(8, 9, 10, 11, 13, 15)) {
            assertEquals(i, secondMerger.mergeStep())
            mergerEnvironment.keyYielded()
        }
        assertEquals(null, secondMerger.mergeStep())
        assertTrue(secondMerger.getRemainingBatches().isEmpty())
    }

    @Test
    fun simpleFailureTest() {
        val dataHolderEnvironment = DataHolderEnvironmentImpl(batchSize = 3)
        val dataHolders = listOf(
            DataHolderImpl(
                dataHolderEnvironment = dataHolderEnvironment,
                keys = listOf(1, 3, 5, 7, 9, 11, 13, 15)
            ),
            DataHolderImpl(
                dataHolderEnvironment = dataHolderEnvironment,
                keys = listOf(2, 4, 6, 8, 10)
            )
        )
        dataHolders.forEach { it.checkpoint() }
        val mergerEnvironment = MergerEnvironmentImpl(dataHolders, dataHolderEnvironment)
        mergerEnvironment.checkpoint()

        mergerEnvironment.newMergerStarted(null)
        val firstMerger = MergerImpl(mergerEnvironment, null)
        for (i in 1..5) {
            assertEquals(i, firstMerger.mergeStep())
            mergerEnvironment.keyYielded()
        }

        val remaining = firstMerger.getRemainingBatches()
        assertEquals(
            mapOf(
                Pair(0, listOf(7, 9, 11)),
                Pair(1, listOf(6))
            ),
            remaining
        )
        dataHolders.forEach { it.checkpoint() }

        val secondMerger = MergerImpl(mergerEnvironment, remaining)
        mergerEnvironment.newMergerStarted(remaining)
        for (i in listOf(6, 7, 8, 9)) {
            assertEquals(i, secondMerger.mergeStep())
            mergerEnvironment.keyYielded()
        }
        dataHolders.forEach { it.rollBack() }
        mergerEnvironment.rollback()

        val thirdMerger = MergerImpl(mergerEnvironment, remaining)
        mergerEnvironment.newMergerStarted(remaining)
        for (i in listOf(6, 7, 8, 9, 10, 11, 13, 15)) {
            assertEquals(i, thirdMerger.mergeStep())
            mergerEnvironment.keyYielded()
        }
        assertEquals(null, thirdMerger.mergeStep())
        assertTrue(thirdMerger.getRemainingBatches().isEmpty())
    }

    private fun getDataHolders(
        keys: List<Int>, random: Random, dataHolderEnvironment: DataHolderEnvironment,
        keysPerDataHolderFrom: Int, keysPerDataHolderUntil: Int
    ): List<DataHolder<Int>> {
        var keysUsed = 0
        val dataHolders = mutableListOf<DataHolder<Int>>()
        while (keysUsed < keys.size) {
            val curUsedKeys = random.nextInt(from = keysPerDataHolderFrom, until = keysPerDataHolderUntil)
            val untilIdx = minOf(keysUsed + curUsedKeys, keys.size)
            val curKeys = keys.subList(fromIndex = keysUsed, toIndex = untilIdx)
            val curDataHolder = DataHolderImpl(dataHolderEnvironment = dataHolderEnvironment, keys = curKeys)
            dataHolders.add(curDataHolder)
            keysUsed = untilIdx
        }
        assert(dataHolders.isNotEmpty())
        return dataHolders.toList()
    }

    private fun doSingleTest(
        random: Random,
        keysCountFrom: Int, keysCountUntil: Int,
        keysFrom: Int, keysUntil: Int,
        batchSizeFrom: Int, batchSizeUntil: Int,
        keysPerDataHolderFrom: Int, keysPerDataHolderUntil: Int,
        keysPerMergerFrom: Int, keysPerMergerUntil: Int,
        failureProb: Double
    ) {
        val batchSize = random.nextInt(from = batchSizeFrom, until = batchSizeUntil)
        val dataHolderEnvironment = DataHolderEnvironmentImpl(batchSize = batchSize)

        val keysCount = random.nextInt(from = keysCountFrom, until = keysCountUntil)
        val keys = (1..keysCount).map { random.nextInt(from = keysFrom, until = keysUntil) }.sorted()

        val dataHolders = getDataHolders(
            keys, random, dataHolderEnvironment,
            keysPerDataHolderFrom = keysPerDataHolderFrom, keysPerDataHolderUntil = keysPerDataHolderUntil
        )
        val mergerEnvironment = MergerEnvironmentImpl(
            dataHolderEnvironment = dataHolderEnvironment,
            dataHolders = dataHolders
        )

        dataHolders.forEach { it.checkpoint() }
        mergerEnvironment.checkpoint()

        val yieldedKeys = mutableListOf<Int>()
        var prevStepBatches: Map<Int, List<Int>>? = null

        loop@ while (true) {
            val curMergerKeys = mutableListOf<Int>()
            val keysPerCurMerger = random.nextInt(from = keysPerMergerFrom, until = keysPerMergerUntil)
            mergerEnvironment.newMergerStarted(prevStepBatches)
            val curMerger = MergerImpl(mergerEnvironment = mergerEnvironment, prevStepBatches = prevStepBatches)

            val failedOp = if (random.nextDouble(from = 0.0, until = 1.0) < failureProb) {
                random.nextInt(from = 1, until = keysPerCurMerger + 1)
            } else {
                null
            }

            for (i in 1..keysPerCurMerger) {
                if (i == failedOp) {
                    dataHolders.forEach { it.rollBack() }
                    mergerEnvironment.rollback()
                    continue@loop
                }

                val curKey = curMerger.mergeStep()
                if (curKey == null) {
                    assertTrue(curMerger.getRemainingBatches().isEmpty())
                    if (mergerEnvironment.curKeysCount != 0) {
                        throw IllegalStateException("Merger process still has keys to yield")
                    }

                    if (failedOp != null) {
                        dataHolders.forEach { it.rollBack() }
                        mergerEnvironment.rollback()
                        continue@loop
                    } else {
                        yieldedKeys.addAll(curMergerKeys)
                        break@loop
                    }
                } else {
                    curMergerKeys.add(curKey)
                    mergerEnvironment.keyYielded()
                }
            }

            if (failedOp == null) {
                yieldedKeys.addAll(curMergerKeys)
                dataHolders.forEach { it.checkpoint() }
                mergerEnvironment.checkpoint()
                val remainingBatches = curMerger.getRemainingBatches()
                if (remainingBatches.map { it.value.isEmpty() }.any { it }) {
                    throw IllegalStateException("Returned mapping $remainingBatches has empty lists in it")
                }
                prevStepBatches = remainingBatches
            }
        }

        assertEquals(keys, yieldedKeys.toList())
    }

    @Suppress("SameParameterValue")
    private fun doStress(
        testsCount: Int,
        keysCountFrom: Int, keysCountUntil: Int,
        keysFrom: Int, keysUntil: Int,
        batchSizeFrom: Int, batchSizeUntil: Int,
        keysPerDataHolderFrom: Int, keysPerDataHolderUntil: Int,
        keysPerMergerFrom: Int, keysPerMergerUntil: Int,
        failureProb: Double
    ) {
        require(
            keysCountFrom in 1 until keysCountUntil &&
                    batchSizeFrom in 1 until batchSizeUntil &&
                    keysFrom < keysUntil &&
                    keysPerDataHolderFrom in 1 until keysPerDataHolderUntil &&
                    keysPerMergerFrom in 1 until keysPerMergerUntil &&
                    0.0 <= failureProb && failureProb <= 1.0
        )
        val random = Random(System.currentTimeMillis())
        repeat(testsCount) {
            doSingleTest(
                random = random,
                keysCountFrom = keysCountFrom, keysCountUntil = keysCountUntil,
                keysFrom = keysFrom, keysUntil = keysUntil,
                batchSizeFrom = batchSizeFrom, batchSizeUntil = batchSizeUntil,
                keysPerDataHolderFrom = keysPerDataHolderFrom, keysPerDataHolderUntil = keysPerDataHolderUntil,
                keysPerMergerFrom = keysPerMergerFrom, keysPerMergerUntil = keysPerMergerUntil,
                failureProb = failureProb
            )
        }
    }

    @Test
    fun stressTestNoFailuresAverage() {
        doStress(
            testsCount = 10000,
            keysCountFrom = 5, keysCountUntil = 1000,
            keysFrom = 0, keysUntil = 1_000_000,
            batchSizeFrom = 1, batchSizeUntil = 50,
            keysPerDataHolderFrom = 20, keysPerDataHolderUntil = 200,
            keysPerMergerFrom = 20, keysPerMergerUntil = 200,
            failureProb = 0.0
        )
    }

    @Test
    fun stressTestNoFailuresSmallBatchSize() {
        doStress(
            testsCount = 10000,
            keysCountFrom = 500, keysCountUntil = 1000,
            keysFrom = 0, keysUntil = 1_000_000,
            batchSizeFrom = 1, batchSizeUntil = 3,
            keysPerDataHolderFrom = 20, keysPerDataHolderUntil = 200,
            keysPerMergerFrom = 20, keysPerMergerUntil = 200,
            failureProb = 0.0
        )
    }

    @Test
    fun stressTestNoFailuresBigBatchSize() {
        doStress(
            testsCount = 10000,
            keysCountFrom = 5, keysCountUntil = 1000,
            keysFrom = 0, keysUntil = 1_000_000,
            batchSizeFrom = 75, batchSizeUntil = 150,
            keysPerDataHolderFrom = 20, keysPerDataHolderUntil = 200,
            keysPerMergerFrom = 20, keysPerMergerUntil = 200,
            failureProb = 0.0
        )
    }

    @Test
    fun stressTestNoFailuresFewKeysPerMerger() {
        doStress(
            testsCount = 10000,
            keysCountFrom = 5, keysCountUntil = 1000,
            keysFrom = 0, keysUntil = 1_000_000,
            batchSizeFrom = 1, batchSizeUntil = 50,
            keysPerDataHolderFrom = 20, keysPerDataHolderUntil = 200,
            keysPerMergerFrom = 1, keysPerMergerUntil = 10,
            failureProb = 0.0
        )
    }

    @Test
    fun stressTestNoFailuresManyKeysPerMerger() {
        doStress(
            testsCount = 10000,
            keysCountFrom = 5, keysCountUntil = 5000,
            keysFrom = 0, keysUntil = 1_000_000,
            batchSizeFrom = 1, batchSizeUntil = 50,
            keysPerDataHolderFrom = 20, keysPerDataHolderUntil = 200,
            keysPerMergerFrom = 300, keysPerMergerUntil = 500,
            failureProb = 0.0
        )
    }

    @Test
    fun stressTestNoFailuresFewKeysPerDataHolder() {
        doStress(
            testsCount = 10000,
            keysCountFrom = 5, keysCountUntil = 1000,
            keysFrom = 0, keysUntil = 1_000_000,
            batchSizeFrom = 1, batchSizeUntil = 50,
            keysPerDataHolderFrom = 1, keysPerDataHolderUntil = 10,
            keysPerMergerFrom = 20, keysPerMergerUntil = 200,
            failureProb = 0.0
        )
    }

    @Test
    fun stressTestNoFailuresManyKeysPerDataHolder() {
        doStress(
            testsCount = 10000,
            keysCountFrom = 5, keysCountUntil = 5000,
            keysFrom = 0, keysUntil = 1_000_000,
            batchSizeFrom = 1, batchSizeUntil = 50,
            keysPerDataHolderFrom = 300, keysPerDataHolderUntil = 500,
            keysPerMergerFrom = 20, keysPerMergerUntil = 200,
            failureProb = 0.0
        )
    }

    @Test
    fun stressTestFailuresAverage() {
        doStress(
            testsCount = 10000,
            keysCountFrom = 5, keysCountUntil = 1000,
            keysFrom = 0, keysUntil = 1_000_000,
            batchSizeFrom = 1, batchSizeUntil = 50,
            keysPerDataHolderFrom = 20, keysPerDataHolderUntil = 200,
            keysPerMergerFrom = 20, keysPerMergerUntil = 200,
            failureProb = 0.3
        )
    }

    @Test
    fun stressTestFailuresSmallBatchSize() {
        doStress(
            testsCount = 10000,
            keysCountFrom = 500, keysCountUntil = 1000,
            keysFrom = 0, keysUntil = 1_000_000,
            batchSizeFrom = 1, batchSizeUntil = 3,
            keysPerDataHolderFrom = 20, keysPerDataHolderUntil = 200,
            keysPerMergerFrom = 20, keysPerMergerUntil = 200,
            failureProb = 0.3
        )
    }

    @Test
    fun stressTestFailuresBigBatchSize() {
        doStress(
            testsCount = 10000,
            keysCountFrom = 5, keysCountUntil = 1000,
            keysFrom = 0, keysUntil = 1_000_000,
            batchSizeFrom = 75, batchSizeUntil = 150,
            keysPerDataHolderFrom = 20, keysPerDataHolderUntil = 200,
            keysPerMergerFrom = 20, keysPerMergerUntil = 200,
            failureProb = 0.3
        )
    }

    @Test
    fun stressTestFailuresFewKeysPerMerger() {
        doStress(
            testsCount = 10000,
            keysCountFrom = 5, keysCountUntil = 1000,
            keysFrom = 0, keysUntil = 1_000_000,
            batchSizeFrom = 1, batchSizeUntil = 50,
            keysPerDataHolderFrom = 20, keysPerDataHolderUntil = 200,
            keysPerMergerFrom = 1, keysPerMergerUntil = 10,
            failureProb = 0.3
        )
    }

    @Test
    fun stressTestFailuresManyKeysPerMerger() {
        doStress(
            testsCount = 10000,
            keysCountFrom = 5, keysCountUntil = 5000,
            keysFrom = 0, keysUntil = 1_000_000,
            batchSizeFrom = 1, batchSizeUntil = 50,
            keysPerDataHolderFrom = 20, keysPerDataHolderUntil = 200,
            keysPerMergerFrom = 300, keysPerMergerUntil = 500,
            failureProb = 0.3
        )
    }

    @Test
    fun stressTestFailuresFewKeysPerDataHolder() {
        doStress(
            testsCount = 10000,
            keysCountFrom = 5, keysCountUntil = 1000,
            keysFrom = 0, keysUntil = 1_000_000,
            batchSizeFrom = 1, batchSizeUntil = 50,
            keysPerDataHolderFrom = 1, keysPerDataHolderUntil = 10,
            keysPerMergerFrom = 20, keysPerMergerUntil = 200,
            failureProb = 0.3
        )
    }

    @Test
    fun stressTestFailuresManyKeysPerDataHolder() {
        doStress(
            testsCount = 10000,
            keysCountFrom = 5, keysCountUntil = 5000,
            keysFrom = 0, keysUntil = 1_000_000,
            batchSizeFrom = 1, batchSizeUntil = 50,
            keysPerDataHolderFrom = 300, keysPerDataHolderUntil = 500,
            keysPerMergerFrom = 20, keysPerMergerUntil = 200,
            failureProb = 0.3
        )
    }
}