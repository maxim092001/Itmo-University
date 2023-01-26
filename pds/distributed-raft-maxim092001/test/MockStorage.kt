package raft.test

import raft.*
import kotlin.random.*

class MockStorage(
    private val actions: ActionSink,
    env: Environment,
    rnd: Random,
    persistentState: PersistentState,
    lastLogIndex: Int,
) : Storage(persistentState) {
    init {
        var term = 1
        for (index in 1..lastLogIndex) {
            term = rnd.nextInt(term..persistentState.currentTerm)
            super.appendLogEntry(rnd.nextLogEntry(index, term, env))
        }
    }

    override fun writePersistentState(state: PersistentState) {
        if (readPersistentState() == state) return
        super.writePersistentState(state)
        actions.removeActionIf { it is ProcessAction.WritePersistentState }
        actions += ProcessAction.WritePersistentState(state)
    }

    override fun appendLogEntry(entry: LogEntry) {
        super.appendLogEntry(entry)
        actions += ProcessAction.AppendLogEntry(entry)
    }

    override fun toString(): String = buildString {
        for (index in 1..readLastLogId().index) {
            if (index > 1) append("\n")
            append(readLog(index))
        }
    }
}