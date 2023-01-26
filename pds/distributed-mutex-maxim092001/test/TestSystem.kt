package mutex.system

import java.util.*
import java.util.concurrent.locks.*
import kotlin.concurrent.*
import kotlin.math.*

class TestSystem(vararg args: String) : DistributedSystem(args) {
    private val dumpVectorClocks = false
    
    @Volatile
    private var failed = false

    private val sysLock = ReentrantLock()
    private val sysCond = sysLock.newCondition()!!

    private val listening = BooleanArray(nodes.size + 1)
    private val pong = BooleanArray(nodes.size + 1)
    private var messageCount = 0
    
    private val vc = Array(nodes.size + 1) { VectorClock(nodes.size) }
    private val sent = Array(nodes.size + 1) { Array(nodes.size + 1) { ArrayDeque<VectorClock>() } }
    private val pendingActions = Array(nodes.size + 1) { ArrayDeque<String>() }

    private val locked = BooleanArray(nodes.size + 1)
    private val lockedVs = Array(nodes.size + 1) { ArrayDeque<VectorClock>() }
    private var lastLocked = VectorClock(nodes.size)
    private var lastLockedIndex = 0

    fun checkNotFailed() {
        check(!failed) { "The test had failed" }
    }

    fun reqLock(id: Int) {
        checkNotFailed()
        request(id, "lock")
    }

    fun reqUnlock(id: Int) {
        checkNotFailed()
        request(id, "unlock")
    }

    fun awaitListening() = sysLock.withLock {
        while (listening.drop(1).any { !it }) {
            checkNotFailed()
            sysCond.await()
        }
    }

    fun awaitPingPongAndCountMessages(): Int = sysLock.withLock {
        for (i in 1..nodes.size) {
            pong[i] = false
            request(i, "ping")
        }
        while (pong.drop(1).any { !it }) {
            checkNotFailed()
            sysCond.await()
        }
        return messageCount.also { messageCount = 0 }
    }

    fun awaitAnyLock(): Int = sysLock.withLock {
        while (locked.all { !it }) {
            checkNotFailed()
            sysCond.await()
        }
        return locked.indexOfFirst { it }
    }

    fun awaitLock(id: Int) = sysLock.withLock {
        while (!locked[id]) {
            checkNotFailed()
            sysCond.await()
        }
    }

    fun awaitUnlock(id: Int) = sysLock.withLock {
        while (locked[id]) {
            checkNotFailed()
            sysCond.await()
        }
    }

    override fun onAction(id: Int, action: String): Unit = sysLock.withLock {
        try {
            if (!pendingActions[id].isEmpty() || !onActionImpl(id, action)) {
                if (dumpVectorClocks) println("-- {$action} pending")
                pendingActions[id].addLast(action) // cannot process yet -- move to pending
            }
            retryPendingImpl() // retry all possible pending
        } catch (e: Throwable) {
            log.error("Failed while processing action at $id: {$action}", e)
            failed = true
            sysCond.signalAll()
        }
    }

    private fun retryPendingImpl() {
        do {
            var changes = false
            for (i in 1..nodes.size) {
                val deque = pendingActions[i]
                while (!deque.isEmpty()) {
                    if (!onActionImpl(i, deque.first!!)) break
                    deque.removeFirst()
                    changes = true
                }
            }
        } while (changes)
    }

    private fun onActionImpl(id: Int, action: String): Boolean {
        val s = action.split(' ')
        val idInAction = s.getOrNull(0)?.toIntOrNull()
        check(idInAction == id) { "Action string {$action} shall start with process id $id" }
        var expectedSize = 2
        when (s.getOrNull(1)) {
            "LISTENING" -> {
                listening[id] = true
                sysCond.signalAll()
            }
            "SEND" -> {
                val remoteId = s.getOrNull(2)?.toIntOrNull()
                    ?: error("Action {$action} shall include remoteId")
                expectedSize = 3
                tick(id)
                sent[id][remoteId].add(vc[id].copy())
                messageCount++
            }
            "RCVD" -> {
                val remoteId = s.getOrNull(2)?.toIntOrNull()
                    ?: error("Action {$action} shall include remoteId")
                expectedSize = 3
                val incomingVec = sent[remoteId][id].pollFirst()
                if (incomingVec == null) return false // not yet.... (wait for it)
                vc[id].updateMax(incomingVec)
                tick(id)
            }
            "LOCKED" -> {
                check(!locked[id])
                locked[id] = true
                tick(id)
                sysCond.signalAll()
            }
            "UNLOCKED" -> {
                check(locked[id])
                locked[id] = false
                tick(id)
                sysCond.signalAll()
            }
            "PONG" -> {
                pong[id] = true
                sysCond.signalAll()
            }
            "ERROR" -> error("Process $id reports error")
            else -> error("Unrecognized action {$action}")
        }
        if (dumpVectorClocks) println("-- {$action} ${vc[id]}")
        check(s.size == expectedSize) { "Action {$action} shall consists of $expectedSize tokens" }
        return true
    }

    override fun awaitTermination() {
        super.awaitTermination()
        checkMutualExclusion(final = true)
    }

    private fun tick(id: Int) {
        val curVec = vc[id]
        curVec[id]++
        if (locked[id]) {
            lockedVs[id].add(curVec.copy())
            checkMutualExclusion(final = false)
        }
    }

    private fun checkMutualExclusion(final: Boolean) {
        while (true) {
            var nextLocked: VectorClock? = null
            var nextLockedIndex = 0
            var count = 0
            for (i in 1..nodes.size) {
                val v = lockedVs[i].firstOrNull() ?: continue
                check(lastLocked hb v) {
                    "Mutual exclusion is violated by lock at process $i on $v.\n\t\t" +
                            "Concurrent with previous lock at process $lastLockedIndex on $lastLocked."
                }
                if (nextLocked == null || v hb nextLocked) {
                    nextLocked = v
                    nextLockedIndex = i
                }
                count++
            }
            if (count == nodes.size || final && count > 0) {
                lockedVs[nextLockedIndex].removeFirst()
                lastLocked = nextLocked!!
                lastLockedIndex = nextLockedIndex
            } else {
                break // nothing more
            }
        }
    }

}

class VectorClock(
    private val nNodes: Int,
    private val v: IntArray = IntArray(nNodes)
) {
    operator fun get(i: Int) = v[i - 1]
    operator fun set(i: Int, x: Int) { v[i - 1] = x }

    fun copy() = VectorClock(nNodes, v.copyOf())

    override fun toString(): String = "VectorClock${v.toList()}"

    fun updateMax(other: VectorClock) {
        for (i in 0 until nNodes) {
            v[i] = max(v[i], other.v[i])
        }
    }

    infix fun hb(other: VectorClock): Boolean {
        check(nNodes == other.nNodes)
        for (i in 0 until nNodes) {
            if (v[i] > other.v[i]) return false
        }
        return true
    }
}
