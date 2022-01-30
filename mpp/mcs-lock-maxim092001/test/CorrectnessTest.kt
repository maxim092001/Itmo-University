@file:Suppress("PLATFORM_CLASS_MAPPED_TO_KOTLIN")

import org.junit.*
import kotlin.random.*

class CorrectnessTest : TestBase(), Environment {
    @Test(timeout = 3000)
    fun testFCFS() {
        val rs = start(4, Solution(this))
        val rnd = Random(1)
        repeat(10) {
            // take random shuffle
            val (p, q, r, s) = rs.shuffled(rnd)
            // request p, then others, ensure FCFS
            p.requestLock()
            p.awaitCritical()
            q.requestLock()
            q.awaitParked()
            r.requestLock()
            r.awaitParked()
            s.requestLock()
            s.awaitParked()
            p.requestUnlock()
            p.awaitNormal()
            q.awaitCritical()
            q.requestUnlock()
            q.awaitNormal()
            r.awaitCritical()
            r.requestUnlock()
            r.awaitNormal()
            s.awaitCritical()
            s.requestUnlock()
            s.awaitNormal()
            // request random one alone and release
            repeat(rnd.nextInt(2)) {
                val t = rs.random(rnd)
                t.requestLock()
                t.awaitCritical() // should not park!
                t.requestUnlock()
                t.awaitNormal()
            }
        }
        stop(rs)
    }

    @Test(timeout = 3000)
    fun testSpuriousWakeup() {
        val rs = start(2, Solution(this))
        val rnd = Random(1)
        repeat(100) {
            val (p, q) = rs.shuffled(rnd)
            p.requestLock()
            p.awaitCritical()
            q.requestLock()
            repeat(10) {
                q.awaitParked()
                q.unpark() // spurious wakeup
            }
            q.awaitParked()
            p.requestUnlock()
            p.awaitNormal()
            q.awaitCritical()
            q.requestUnlock()
            q.awaitNormal()
        }
        stop(rs)
    }

    private fun <N> start(n: Int, lock: Lock<N>): List<Runner<*>> {
        val rs = List<Runner<*>>(n) { i -> Runner(lock, "Runner-$i") }
        rs.configureExceptionHandlers()
        rs.forEach { it.start() }
        return rs
    }

    private fun stop(rs: List<Runner<*>>) {
        rs.forEach { it.interrupt() }
        rs.forEach { it.join() }
    }

    enum class Phase { Normal, RequestLock, Parked, Unparked, Critical, RequestUnlock }

    inner class Runner<N>(val lock: Lock<N>, name: String) : Thread(name) {
        private var phase = Phase.Normal

        fun requestLock() = transition(Phase.Normal, Phase.RequestLock)
        fun awaitCritical() = awaitPhase(Phase.Critical)
        fun awaitParked() = awaitPhase(Phase.Parked)
        fun requestUnlock() = transition(Phase.Critical, Phase.RequestUnlock)
        fun awaitNormal() = awaitPhase(Phase.Normal)

        @Synchronized
        fun awaitPhase(vararg ps: Phase) {
            while (phase !in ps) (this as Object).wait()
        }

        @Synchronized
        fun transition(p: Phase, q: Phase) {
            require(phase == p) { "Expected to be in phase $p" }
            phase = q
            (this as Object).notifyAll()
        }

        @Synchronized
        fun transition(p1: Phase, p2: Phase, q: Phase) {
            require(phase == p1 || phase == p2) { "Expected to be in phase $p1 or $p2" }
            phase = q
            (this as Object).notifyAll()
        }

        override fun run() {
            try {
                while (true) {
                    awaitPhase(Phase.RequestLock)
                    lock.checkLock {
                        transition(Phase.RequestLock, Phase.Unparked, Phase.Critical)
                        awaitPhase(Phase.RequestUnlock)
                    }
                    transition(Phase.RequestUnlock, Phase.Normal)
                }
            } catch (e: InterruptedException) { /* ok */ }
        }

        fun park() {
            transition(Phase.RequestLock, Phase.Unparked, Phase.Parked)
            awaitPhase(Phase.Unparked)
        }

        fun unpark() {
            awaitPhase(Phase.Parked, Phase.Unparked)
            transition(Phase.Parked, Phase.Unparked, Phase.Unparked)
        }
    }

    override fun park() {
        (Thread.currentThread() as Runner<*>).park()
    }

    override fun unpark(thread: Thread) {
        (thread as Runner<*>).unpark()
    }
}