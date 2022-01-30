import Operation.*
import java.util.*
import java.util.concurrent.*
import java.util.concurrent.atomic.*
import kotlin.math.*
import kotlin.test.*
import kotlin.random.Random

private const val N = 10 // that is a number of accounts bank is going to have
private const val RUN_ACCOUNTS = 3 // each run will touch this # of accounts
private const val RUNS = 100
private const val MIN_THREADS = 2
private const val MAX_THREADS = 3
private const val MAX_OPS_PER_THREAD = 2 // Max ops per threads
private const val MAX_OPS_PER_RUN = 5 // Max ops total NOTE:WARNING: Will try almost MAX_OPS_PER_RUN! (factorial) serial executions
private const val MIN_EXECUTIONS = 1000 // min executions per run
private const val MAX_EXECUTIONS = 1000000 // max executions per run (if not all results seen yet)
private const val MIN_FUZZ = 0
private const val MAX_FUZZ = 100 // max # of CPU consuming spins before operations
private const val MIN_RESULTS = 3 // don't run trivial combos of operations

/**
 * Automated test of linearizability of multi-threaded bank implementation.
 */
class LinearizabilityTest {
    private val rnd = Random(20191217)
    private val phaser = Phaser(1) // will register additional threads as they are created
    private val runAccounts = IntArray(RUN_ACCOUNTS)
    private val baseAmount = LongArray(RUN_ACCOUNTS)
    private var allOpsCnt = 0
    private val allOps = arrayOfNulls<Operation>(MAX_OPS_PER_RUN)
    private val threadOpsCnt = IntArray(MAX_THREADS)
    private val threadOps = Array(MAX_THREADS) { IntArray(MAX_OPS_PER_THREAD) }

    private lateinit var bank: Bank
    private var nThreads = 0 // current number of threads = 0
    private var runNo = 0 // current run number = 0
    private var needFuzz = 0// current fuzzing limit (if needed), up to MAX_FUZZ = 0
    
    private val resultsHash = ResultsHash()
    private val results = Results(MAX_OPS_PER_RUN) // allocate results for MAX_OPS_PER_RUN
    private var sumTotalResults = 0
    private var sumSeenResults = 0

    @Test
    fun testLinearizability() {
        nThreads = 1
        while (nThreads <= MAX_THREADS) {
            phaser.register()
            TestThread(nThreads - 1).start() // add a thread
            if (nThreads < MIN_THREADS) {
                nThreads++
                continue
            }
            runNo = 1
            while (runNo <= RUNS) {
                doOneRun()
                runNo++
            }
            nThreads++
        }
        dumpSumStats()
    }

    @AfterTest
    fun tearDown() {
        phaser.forceTermination()
    }

    private fun doOneRun() {
        createNonTrivialRun()
        var printedWorking = false
        for (i in 0 until MAX_EXECUTIONS) {
            if (i >= MIN_EXECUTIONS) {
                if (resultsHash.seenCount == resultsHash.totalCount) break // break when already seen all and min executions done
                if (!printedWorking) {
                    printedWorking = true
                    dumpRunStats("working")
                }
            }
            needFuzz = min(MAX_FUZZ, MIN_FUZZ + i / MIN_EXECUTIONS)
            doOneExecution()
            if (!resultsHash.findResultsAndCountSeen(results)) {
                dumpNonLinearizableError()
                fail("Non-linearizable")
            }
        }
        dumpRunStats("completed")
        if (resultsHash.seenCount < resultsHash.totalCount) {
            dumpIncompleteWarning()
        }
        sumTotalResults += resultsHash.totalCount
        sumSeenResults += resultsHash.seenCount
    }

    private fun dumpIncompleteWarning() {
        println()
        println("===================================================")
        println("WARNING: Run failed to produce all possible results")
        dumpRun(false)
    }

    private fun dumpNonLinearizableError() {
        println()
        println("=======================================")
        println("ERROR: Non-linearizable execution found")
        dumpRun(true)
    }

    private fun createNonTrivialRun() {
        var maxPossibleResults = 1
        for (i in 2..nThreads) maxPossibleResults *= i
        do {
            createRandomRun()
            resultsHash.clear()
            results.size = allOpsCnt
            serialScan(0, 0, IntArray(allOpsCnt))
        } while (resultsHash.totalCount < min(MIN_RESULTS, maxPossibleResults))
    }

    private fun createRandomRun() {
        for (i in 0 until RUN_ACCOUNTS) {
            var ok: Boolean
            do {
                runAccounts[i] = rnd.nextInt(N)
                ok = true
                for (j in 0 until i) if (runAccounts[i] == runAccounts[j]) ok = false
            } while (!ok)
            baseAmount[i] = nextRndAmount()
        }
        allOpsCnt = 0
        for (t in 0 until nThreads) {
            val maxOpsCnt = min(MAX_OPS_PER_THREAD, MAX_OPS_PER_RUN - allOpsCnt - (nThreads - t - 1))
            val opsCnt = rnd.nextInt(maxOpsCnt) + 1
            threadOpsCnt[t] = opsCnt
            for (q in 0 until opsCnt) {
                val k = allOpsCnt++
                allOps[k] = randomOperation()
                threadOps[t][q] = k
            }
        }
    }

    private fun randomOperation(): Operation =
        when (rnd.nextInt(5)) {
            0 -> GetAmount(nextRndRunAccount())
            1 -> GetTotalAmount()
            2 -> Deposit(nextRndRunAccount(), nextRndAmountOrInvalid())
            3 -> Withdraw(nextRndRunAccount(), nextRndAmountOrInvalid())
            4 -> {
                var i: Int
                var j: Int
                do {
                    i = nextRndRunAccount()
                    j = nextRndRunAccount()
                } while (i == j)
                Transfer(i, j, nextRndAmountOrInvalid())
            }
            else -> throw AssertionError()
        }

    private fun dumpRunStats(state: String) {
        println("Using $nThreads/$MAX_THREADS threads run $runNo/$RUNS $state, " +
                "seen ${resultsHash.seenCount} out of ${resultsHash.totalCount} results ...")
    }

    private fun dumpSumStats() {
        println("All runs completed, seen $sumSeenResults out of $sumTotalResults " +
                "(${(100.0 * sumSeenResults / sumTotalResults).roundToInt()}%) results")
    }

    private fun dumpRun(withRunResults: Boolean) {
        println("Initial state:")
        for (i in 0 until RUN_ACCOUNTS) {
            println("  Account #${runAccounts[i]} with amount ${baseAmount[i]}")
        }
        println("Operations:")
        for (t in 0 until nThreads) for (q in 0 until threadOpsCnt[t]) {
            val k = threadOps[t][q]
            println("  [thread $t, op $q] ${allOps[k]}${if (withRunResults) " with result ${results[k]}" else ""}")
        }
        println("All valid results:")
        resultsHash.dump()
    }

    private fun serialScan(i: Int, used: Int, order: IntArray) {
        if (i >= allOpsCnt) {
            initBank(SequentialBank(N))
            for (k in 0 until allOpsCnt) results[order[k]] = allOps[order[k]]!!.invoke(bank)
            resultsHash.registerResults(results)
            return
        }
        for (t in 0 until nThreads) {
            for (q in 0 until threadOpsCnt[t]) {
                val k = threadOps[t][q]
                if (used and (1 shl k) == 0) {
                    order[i] = k
                    serialScan(i + 1, used or (1 shl k), order)
                    break // try next thread, thread ops only in order
                }
            }
        }
    }

    private fun doOneExecution() {
        initBank(BankImpl(N))
        phaser.arriveAndAwaitAdvance()
        phaser.arriveAndAwaitAdvance()
    }

    private fun initBank(bank: Bank) {
        this.bank = bank
        for (i in 0 until RUN_ACCOUNTS) bank.deposit(runAccounts[i], baseAmount[i])
    }

    private fun nextRndRunAccount(): Int {
        return runAccounts[rnd.nextInt(RUN_ACCOUNTS)]
    }

    private fun nextRndAmountOrInvalid(): Long {
        if (rnd.nextInt(100) == 0) { // 1% of invalid amounts
            when (rnd.nextInt(6)) {
                0 -> return 0
                1 -> return -1
                2 -> return Long.MIN_VALUE
                3 -> return MAX_AMOUNT + 1
                4 -> return MAX_AMOUNT + 2
                5 -> return Long.MAX_VALUE
            }
        }
        return nextRndAmount()
    }

    private fun nextRndAmount(): Long {
        val base = 1000000000
        return 1 + rnd.nextInt(base) + rnd.nextInt((MAX_AMOUNT / base).toInt()) * base.toLong()
    }

    private inner class TestThread(private val threadNo: Int) : Thread("TestThread-$threadNo") {
        private val cpuConsumer = AtomicInteger()

        override fun run() {
            while (true) {
                if (phaser.arriveAndAwaitAdvance() < 0) return
                fuzzIfNeeded()
                doOneExecution()
                if (phaser.arriveAndAwaitAdvance() < 0) return
            }
        }

        private fun doOneExecution() {
            var q = 0
            while (true) {
                val k = threadOps[threadNo][q]
                results[k] = allOps[k]!!.invoke(bank)
                if (++q >= threadOpsCnt[threadNo]) break
                fuzzIfNeeded()
            }
        }

        private fun fuzzIfNeeded() {
            if (needFuzz > 0) fuzz()
        }

        private fun fuzz() {
            val fuzz = ThreadLocalRandom.current().nextInt(needFuzz)
            for (i in 0 until fuzz) consumeCPU()
        }

        private fun consumeCPU() {
            cpuConsumer.incrementAndGet()
        }
    }
}

/**
 * Results of operations on a bank.
 */
class Results(capacity: Int, other: Results? = null) {
    constructor(other: Results) : this(other.size, other)

    private val results: Array<Any?> = other?.results?.copyOf(capacity) ?: arrayOfNulls(capacity)

    var size = 0
    var count = 0
        private set


    operator fun get(i: Int): Any? = results[i]
    operator fun set(i: Int, v: Any?) { results[i] = v }

    fun incCount() { count++ }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is Results) return false
        for (i in 0 until size) if (results[i] != other.results[i]) return false
        return true
    }

    override fun hashCode(): Int {
        var h = 1
        for (i in 0 until size) h = 31 * h + results[i].hashCode()
        return h
    }

    override fun toString(): String = results.joinToString(separator = ", ", prefix = "[", postfix = "]")
}

/**
 * Hash table of all encountered results.
 */
class ResultsHash {
    private val hash = arrayOfNulls<Results>(SIZE)

    var totalCount = 0
        private set
    var seenCount = 0
        private set

    fun clear() {
        Arrays.fill(hash, null)
        totalCount = 0
        seenCount = 0
    }

    fun registerResults(results: Results) {
        var hIndex = results.hashCode() and SIZE - 1
        while (true) {
            var hRes = hash[hIndex]
            if (hRes == null) {
                hRes = Results(results)
                hash[hIndex] = hRes
                totalCount++
                break
            }
            if (hRes == results) break
            if (hIndex == 0) hIndex = SIZE
            hIndex--
        }
    }

    fun findResultsAndCountSeen(results: Results): Boolean {
        var hIndex = results.hashCode() and SIZE - 1
        while (true) {
            val hRes = hash[hIndex] ?: return false
            if (hRes == results) {
                if (hRes.count == 0) seenCount++
                hRes.incCount()
                return true
            }
            if (hIndex == 0) hIndex = SIZE
            hIndex--
        }
    }

    fun dump() {
        for (results in hash) if (results != null) println("  " + results + " seen " + results.count + " times")
    }

    companion object {
        private const val SIZE = 1024 // must be 2^N;
    }
}