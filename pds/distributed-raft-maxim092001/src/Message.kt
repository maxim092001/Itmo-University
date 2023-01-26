package raft

import kotlinx.serialization.*

/**
 * Raft message types.
 */
@Serializable
sealed class Message {
    /** Current term of the process that was sending the message. */
    abstract val term: Int

    /** AppendEntries RPC request. It is being sent by the leader. */
    @Serializable
    @SerialName("AppendEntryRpc")
    data class AppendEntryRpc(
        /** Leader’s term. */
        override val term: Int,
        /** A pair of `prevLogIndex` and `prevLogTerm`. */
        val prevLogId: LogId,
        /** Leader’s `commitIndex`. */
        val leaderCommit: Int,
        /** Log entry to store, `null` for heartbeat. */
        val entry: LogEntry?
    ) : Message()

    /** AppendEntries result. It is being sent in response to [AppendEntryRpc]. */
    @Serializable
    @SerialName("AppendEntryResult")
    data class AppendEntryResult(
        /** Current term for leader to update itself. */
        override val term: Int,
        /** If success, then `entry.id.index ?: prevLogId.id.index`; `null` when not successful. */
        val lastIndex: Int?,
    ) : Message()

    /** RequestVote RPC request. It is being sent by a candidate. */
    @Serializable
    @SerialName("RequestVoteRpc")
    data class RequestVoteRpc(
        /** Candidate's term */
        override val term: Int,
        /** A pair of `lastLogIndex` and `lastLogTerm`. */
        val lastLogId: LogId
    ) : Message()

    /** RequestVote result. It is being sent in response to [RequestVoteRpc]. */
    @Serializable
    @SerialName("RequestVoteResult")
    data class RequestVoteResult(
        /** Current term for candidate to update itself. */
        override val term: Int,
        /** `true` means candidate received vote. */
        val voteGranted: Boolean
    ) : Message()

    /** Forwards client command to the leader. */
    @Serializable
    @SerialName("ClientCommandRpc")
    data class ClientCommandRpc(
        /** Current term. */
        override val term: Int,
        /** Client command. */
        val command: Command
    ) : Message()

    /** Result of the client command for the original [Command.processId]. Sent by the leader. */
    @Serializable
    @SerialName("ClientCommandResult")
    data class ClientCommandResult(
        /** Current term for the receiving process to update itself. */
        override val term: Int,
        /** Result of the command. */
        val result: CommandResult
    ) : Message()
}