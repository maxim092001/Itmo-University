package mutex

import com.sun.jdi.request.InvalidRequestStateException

/**
 * Distributed mutual exclusion implementation.
 * All functions are called from the single main thread.
 *
 * @author Maksim Grankin
 */
class ProcessImpl(private val env: Environment) : Process {

    private var isCriticalSection: Boolean = false
    private var needCriticalSection: Boolean = false
    private var reqProcesses = 0
    private val processes: IntArray = (0..env.nProcesses + 1).toList().toIntArray()
    private val processStatuses: Array<ProcessStatus> = processes.map {
        when {
            env.processId < it -> ProcessStatus.DONE
            else -> ProcessStatus.OTHER
        }
    }.toTypedArray()
    private val otherProcesses: IntArray =
        processes.dropLast(1).map { it + 1 }.filter { it != env.processId }.toIntArray()
    private val processRequests: Array<Boolean> = processes.map { false }.toTypedArray()

    override fun onMessage(srcId: Int, message: Message) {
        when (message.parse { readEnum<MessageStatus>() }) {
            MessageStatus.REQUEST -> request(srcId)
            MessageStatus.RESPONSE -> response(srcId)
        }
    }

    override fun onLockRequest() {
        needCriticalSection = true
        otherProcesses.forEach {
            if (processStatuses[it] == ProcessStatus.OTHER) {
                env.send(it) {
                    writeEnum(MessageStatus.REQUEST)
                }
                reqProcesses++
            }
        }
        checkReqProcesses(reqProcesses)()
    }

    override fun onUnlockRequest() {
        env.unlocked()
        isCriticalSection = false
        otherProcesses.forEach {
            processStatuses[it] = (if (processRequests[it]) {
                env.send(it) {
                    writeEnum(MessageStatus.RESPONSE)
                }
                processRequests[it] = false
                ProcessStatus.OTHER
            } else {
                ProcessStatus.DONE
            })
        }
    }

    private fun request(srcId: Int) {
        when (processStatuses[srcId]) {
            ProcessStatus.DONE -> {
                when {
                    isCriticalSection -> {
                        processRequests[srcId] = true
                    }
                    else -> {
                        responseOther(srcId)
                        if (needCriticalSection) {
                            reqProcesses++
                            env.send(srcId) {
                                writeEnum(MessageStatus.REQUEST)
                            }
                        }
                    }
                }
            }
            ProcessStatus.IN_WORK ->
                when {
                    isCriticalSection || needCriticalSection -> {
                        processRequests[srcId] = true
                    }
                    else -> responseOther(srcId)
                }
            ProcessStatus.OTHER -> throw InvalidRequestStateException("Process $srcId not owned by consumer")
        }
    }

    private fun response(srcId: Int) {
        processStatuses[srcId] = ProcessStatus.IN_WORK
        checkReqProcesses(--reqProcesses)()
    }

    private fun checkReqProcesses(p: Int): () -> Unit = {
        if (p == 0) {
            env.locked()
            needCriticalSection = false
            isCriticalSection = true
        }
    }

    private fun responseOther(srcId: Int) = run {
        env.send(srcId) {
            writeEnum(MessageStatus.RESPONSE)
        }
        processStatuses[srcId] = ProcessStatus.OTHER
    }

    enum class ProcessStatus { IN_WORK, DONE, OTHER }
    enum class MessageStatus { REQUEST, RESPONSE }
}
