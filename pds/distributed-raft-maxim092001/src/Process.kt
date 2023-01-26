package raft

/**
 * Interface that process implementing Raft algorithm shall satisfy.
 * All functions are called from the single main thread.
 */
interface Process {
    /**
     * Called when timeout passed after [Environment.startTimeout]
     */
    fun onTimeout()

    /**
     * Called on arriving [message] from another process [srcId] (from 1 to [Environment.nProcesses]).
     */
    fun onMessage(srcId: Int, message: Message)

    /**
     * Called on client-requested command. The [Command.processId] is always equal to the
     * identifier of this process ([Environment.processId]).
     */
    fun onClientCommand(command: Command)
}