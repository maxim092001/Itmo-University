package mutex

/**
 * Interface that process implementing mutual exclusion algorithm shall satisfy.
 * All functions are called from the single main thread.
 */
interface Process {
    /**
     * Called on arriving [message] from another process [srcId] (from 1 to [Environment.nProcesses]).
     */
    fun onMessage(srcId: Int, message: Message)

    /**
     * Called on lock request.
     */
    fun onLockRequest()

    /**
     * Called on unlock request.
     */
    fun onUnlockRequest()
}