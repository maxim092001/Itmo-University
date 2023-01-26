package raft.test

import kotlinx.serialization.*
import org.slf4j.*
import raft.*
import raft.system.*
import raft.system.ActionTag.*
import java.util.*
import java.util.concurrent.*
import java.util.concurrent.locks.*
import kotlin.collections.ArrayDeque
import kotlin.concurrent.*

data class SystemCommandResult(
    val processId: Int,
    val result: CommandResult
)

private val testNodeProps = Properties().apply {
    put(HEARTBEAT_TIMEOUT_MS_PROP, "200")
    put(HEARTBEAT_RANDOM_PROP, "true")
    put(STORAGE_FQN_PROP, Storage::class.qualifiedName)
    put(MACHINE_FQN_PROP, DistributedTestStateMachine::class.qualifiedName)
}

private const val AWAIT_TIMEOUT_MS = 5000

class DistributedTestSystem(vararg args: String) : DistributedSystem(testNodeProps, args) {
    @Volatile
    private var failed = false

    private val sysLock = ReentrantLock()
    private val sysCond = sysLock.newCondition()

    private val listening = BooleanArray(nProcesses + 1)
    private val dump = arrayOfNulls<StateMachine>(nProcesses + 1)
    private val commit = arrayOfNulls<Command?>(nProcesses + 1)
    private val restart = BooleanArray(nProcesses + 1)
    private val results = ArrayDeque<SystemCommandResult>()

    fun checkNotFailed() {
        check(!failed) { "The test had failed" }
    }

    private fun <T> await(
        condition: () -> Boolean,
        action: () -> T,
        message: String
    ): T = sysLock.withLock {
        val deadline = System.currentTimeMillis() + AWAIT_TIMEOUT_MS
        while (!condition()) {
            checkNotFailed()
            val now = System.currentTimeMillis()
            if (now >= deadline) error("Test timed out waiting for $message")
            sysCond.await(deadline - now, TimeUnit.MILLISECONDS)
        }
        action()
    }

    fun awaitListening() = await(
        condition = { listening.drop(1).all { it } },
        action = { listening.fill(false) },
        message = "listening"
    )

    fun awaitDump(id: Int) = await(
        condition = { dump[id] != null },
        action = { dump[id]!!.also { dump[id] = null } },
        message = "dump $id"
    )

    fun awaitCommit(id: Int) = await(
        condition = { commit[id] != null },
        action = { commit[id]!!.also { commit[id] = null } },
        message = "commit $id"
    )

    fun awaitClientCommandResult(): SystemCommandResult = await(
        condition = { results.isNotEmpty() },
        action = { results.removeFirst() },
        message = "client command result"
    )

    fun awaitRestart(id: Int) = await(
        condition = { restart[id] },
        action = { restart[id] = false },
        message = "restart $id"
    )

    override fun onAction(id: Int, action: String, attachment: String): Unit = sysLock.withLock {
        try {
            onActionImpl(id, action, attachment)
        } catch (e: Throwable) {
            log.error("Failed while processing action at $id: {$action} $attachment", e)
            failed = true
            sysCond.signalAll()
        }
    }

    private fun onActionImpl(id: Int, action: String, attachment: String) {
        val s = action.split(' ')
        val idInAction = s.getOrNull(0)?.toIntOrNull()
        check(idInAction == id) { "Action string {$action} shall start with process id $id" }
        val actionTag = ActionTag.valueOf(s.getOrNull(1) ?: "")
        when (actionTag) {
            LISTENING -> {
                listening[id] = true
                sysCond.signalAll()
            }
            DUMP -> {
                dump[id] = StateMachine(attachment)
                sysCond.signalAll()
            }
            COMMIT -> {
                commit[id] = json.decodeFromString(attachment)
                sysCond.signalAll()
            }
            RESULT -> {
                results += SystemCommandResult(id, json.decodeFromString(attachment))
                sysCond.signalAll()
            }
            RESTART -> {
                restart[id] = true
                sysCond.signalAll()
            }
            RCVD, SENT, COMMAND, TIMEOUT -> {}
            ERROR -> error("Process $id reports error: $attachment")
        }
        val expectedSize = if (actionTag == RCVD || actionTag == SENT) 3 else 2
        check(s.size == expectedSize) { "Action {${ActionTag.valueOf(s.getOrNull(1) ?: "")}} shall consists of $expectedSize tokens" }
    }
}

class DistributedTestStateMachine(private val processId: Int) : StateMachine() {
    private val log = LoggerFactory.getLogger("Stm")

    override fun apply(command: Command): CommandResult {
        log.info("$ACTION_TAG{$processId ${COMMIT}} ${json.encodeToString(command)}")
        return super.apply(command)
    }
}

