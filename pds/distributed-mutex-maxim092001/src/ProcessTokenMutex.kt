package mutex

/**
 * Token-based Distributed Mutual Exclusion Algorithm implementation..
 * All functions are called from the single main thread.
 */
class ProcessTokenMutex(private val env: Environment) : Process {
    private var status = Status.WAIT
    private var token = 0

    private val nextId = env.processId % env.nProcesses + 1

    init {
        if (env.processId == 1) env.send(nextId) {
            writeInt(1)
        }
    }

    override fun onMessage(srcId: Int, message: Message) {
        token = message.parse { readInt() }
        if (status == Status.REQ) {
            status = Status.CS
            env.locked()
        } else {
            env.send(nextId) {
                writeInt(token + 1)
            }
        }
    }

    override fun onLockRequest() {
        check(status == Status.WAIT)
        status = Status.REQ
    }

    override fun onUnlockRequest() {
        check(status == Status.CS)
        status = Status.WAIT
        env.unlocked()
        env.send(nextId) {
            writeInt(token + 1)
        }
    }

    enum class Status { WAIT, REQ, CS }
}
