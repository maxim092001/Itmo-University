package mutex

import java.util.*

/**
 * Centrally-coordinated Distributed Mutual Exclusion Algorithm implementation.
 * All functions are called from the single main thread.
 */
class ProcessSyncCoordinatedMutex(private val env: Environment) : Process {
    private val coordinatorId = 1
    private val isCoordinator = env.processId == coordinatorId
    private val queue = ArrayDeque<Int>()
    private var inCS = 0
    
    override fun onMessage(srcId: Int, message: Message) {
        val msgType = message.parse { readEnum<MsgType>() }
        when (msgType) {
            MsgType.REQ -> {
                check(isCoordinator)
                queue.addLast(srcId)
                checkCSEnter()
            }
            MsgType.OK -> {
                check(!isCoordinator)
                inCS = env.processId
                env.locked()
            }
            MsgType.REL -> {
                check(isCoordinator)
                check(inCS == srcId)
                inCS = 0
                checkCSEnter()
            }
        }
    }

    override fun onLockRequest() {
        if (isCoordinator) {
            queue.add(coordinatorId)
            checkCSEnter()
        } else {
            send(coordinatorId, MsgType.REQ)
        }
    }

    override fun onUnlockRequest() {
        check(inCS == env.processId)
        inCS = 0
        env.unlocked()
        if (isCoordinator) {
            checkCSEnter()
        } else {
            send(coordinatorId, MsgType.REL)
        }
    }

    private fun checkCSEnter() {
        if (inCS != 0) return
        val id = queue.pollFirst() ?: return
        inCS = id
        if (id == coordinatorId) {
            env.locked()
        } else {
            send(id, MsgType.OK)
        }
    }

    private fun send(destId: Int, type: MsgType) {
        env.send(destId) {
            writeEnum(type)
        }
    }

    enum class MsgType { REQ, OK, REL }
}
