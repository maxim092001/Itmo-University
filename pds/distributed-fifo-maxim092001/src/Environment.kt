package mutex

/**
 * Environment interface for communication with other processes.
 */
interface Environment {
    /**
     * Identifier of this process (from 1 to [nProcesses]).
     */
    val processId: Int

    /**
     * The total number of processes in the system.
     */
    val nProcesses: Int

    /**
     * Sends the specified [message] to the process [destId] (from 1 to [nProcesses]).
     */
    fun send(destId: Int, message: Message)

    /**
     * Indicates that the process had entered critical section.
     * It should be only called after locking was requested via [Process.onLockRequest].
     */
    fun locked()

    /**
     * Indicates that the process had exited critical section.
     * It should be only called after unlocking was requested via [Process.onUnlockRequest].
     */
    fun unlocked()
}

/**
 * Builds the message and sends it to the process [destId].
 */
inline fun Environment.send(destId: Int, builder: MessageBuilder.() -> Unit = {}) =
    send(destId, Message(builder))
