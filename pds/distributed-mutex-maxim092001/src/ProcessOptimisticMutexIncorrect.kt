package mutex

/**
 * This is broken distributed mutual exclusion implementation that is "optimistic" in lock acquisition.
 * All functions are called from the single main thread.
 */
class ProcessOptimisticMutexIncorrect(private val env: Environment) : Process {
    private val requested = BooleanArray(env.nProcesses + 1)
    private var inCS = false

    override fun onMessage(srcId: Int, message: Message) {
        val type = message.parse { readEnum<MsgType>() }
        when (type) {
            MsgType.REQ -> requested[srcId] = true
            MsgType.REL -> requested[srcId] = false
        }
        checkCSEnter()
    }

    override fun onLockRequest() {
        check(!requested[env.processId])
        requested[env.processId] = true
        broadcast(MsgType.REQ)
        checkCSEnter()
    }

    override fun onUnlockRequest() {
        check(inCS)
        inCS = false
        requested[env.processId] = false
        env.unlocked()
        broadcast(MsgType.REL)
    }

    private fun checkCSEnter() {
        if (!requested[env.processId] || inCS) return
        for (i in 1 until env.processId) if (requested[i]) return // give way for lower numbered
        inCS = true
        env.locked()
    }

    private fun broadcast(type: MsgType) {
        for (i in 1..env.nProcesses) {
            if (i != env.processId) {
                env.send(i) {
                    writeEnum(type)
                }
            }
        }
    }

    enum class MsgType { REQ, REL }

}
