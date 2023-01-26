package raft

import kotlinx.serialization.Serializable

/** Identifier of the last entry of an empty log. */
val START_LOG_ID = LogId(0, 0)

/** Unique identifier for a log entry. */
@Serializable
data class LogId(
    /** Index of the log entry, starting from 1. */
    val index: Int,
    /** Term of the log entry. */
    val term: Int
) : Comparable<LogId> {
    override fun compareTo(other: LogId): Int {
        if (term != other.term) return term.compareTo(other.term)
        return index.compareTo(other.index)
    }
}

@Serializable
data class LogEntry(
    val id: LogId,
    val command: Command
)

/** Persistent state of a Raft process. */
@Serializable
data class PersistentState(
    /** Latest term server has seen (initialized to 0 on first boot, increases monotonically). */
    val currentTerm: Int = 0,
    /** CandidateId that received vote in current term (or `null` if none). */
    val votedFor: Int? = null,
)

/**
 * Persistent storage for the Raft algorithm.
 */
open class Storage @JvmOverloads constructor(
    private var state: PersistentState = PersistentState()
) {
    private val logEntries = arrayListOf<LogEntry?>(null)

    /** Reads [PersistentState] of the Raft algorithm. */
    fun readPersistentState(): PersistentState = state

    /** Saves [PersistentState] of the Raft algorithm. */
    open fun writePersistentState(state: PersistentState) {
        this.state = state
    }

    /** Reads identifier of the last log entry. */
    fun readLastLogId(): LogId = readLog(logEntries.lastIndex)?.id ?: START_LOG_ID

    /** Reads log entry at the specified index, return `null` if the entry is not present. */
    fun readLog(index: Int): LogEntry? = logEntries.getOrNull(index)

    /** Appends entry to the log, replacing and dropping all entries with index `entry.id.index` and larger indices. */
    open fun appendLogEntry(entry: LogEntry) {
        val index = entry.id.index
        require(index in 1..logEntries.size) { "Index for $entry must be in range 1..${logEntries.size}" }
        while (logEntries.lastIndex >= index) logEntries.removeLast()
        require(index == logEntries.size)
        logEntries += entry
    }
}
