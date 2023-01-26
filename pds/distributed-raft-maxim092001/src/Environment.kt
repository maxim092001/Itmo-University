package raft

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
     * Starts/restart timeout of a given [Timeout] type. When timeout elapses, [Process.onTimeout] is called.
     */
    fun startTimeout(timeout: Timeout)

    /**
     * Must be called on the client that has originally requested the command to be executed with the
     * result of the command's execution.
     */
    fun onClientCommandResult(result: CommandResult)

    /**
     * Reference to the persistent storage.
     */
    val storage: Storage

    /**
     * Reference to the state machine.
     */
    val machine: StateMachine
}

/**
 * A type of the timeout in the Raft algorithm.
 */
enum class Timeout {
    /**
     * A randomized timeout when waiting for a heartbeat from a leader.
     */
    ELECTION_TIMEOUT,

    /**
     * A fixed timeout at which the leader shall send heatbeats.
     */
    LEADER_HEARTBEAT_PERIOD
}



