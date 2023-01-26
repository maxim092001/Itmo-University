package raft

import raft.Message.*
import java.lang.Integer.min
import java.util.*

/**
 * Raft algorithm implementation.
 * All functions are called from the single main thread.
 *
 * @author Maksim Grankin
 */
class ProcessImpl(env: Environment) : Process {
    private var process: AbstractProcess

    init {
        process = Follower(env, null, this)
        initFollower(process as Follower)
    }

    override fun onTimeout() = process.onTimeout()

    override fun onMessage(srcId: Int, message: Message) = process.onMessage(srcId, message)

    override fun onClientCommand(command: Command) = process.onCommand(command)

    fun changeCurrentStatus(process: AbstractProcess) =
        (when (process) {
            is Candidate -> with(process) {
                currentState.addVote(currentState.environment.processId)
                currentState.setPersistentState(
                    currentState.currentTerm() + 1,
                    currentState.environment.processId
                )
                broadcast(RequestVoteRpc(currentState.currentTerm(), currentState.lastLog()))
                currentState.env.startTimeout(Timeout.ELECTION_TIMEOUT)
            }
            is Follower -> initFollower(process)
            is Leader -> with(process) {
                currentState.env.startTimeout(Timeout.LEADER_HEARTBEAT_PERIOD)
                broadcastHeartbeat()
                currentState.delayedCommands.forEach { onCommand(it) }
                currentState.delayedCommands.clear()
            }
            else -> {}
        }).also { this.process = process }

    private fun initFollower(process: Follower) = with(process) {
        currentState.setVotedFor(null)
        currentState.env.startTimeout(Timeout.ELECTION_TIMEOUT)
    }
}

interface AbstractProcess {

    val currentState: CurrentState
    val externalProcess: ProcessImpl

    fun onTimeout() = externalProcess.changeCurrentStatus(Candidate(currentState.env, currentState, externalProcess))

    fun onMessage(srcId: Int, message: Message) = message.react(srcId)
    fun onCommand(command: Command)

    open class CurrentState(val env: Environment, prevState: CurrentState?) {

        var commitIndex: Int
        var lastApplied: Int
        val delayedCommands: MutableList<Command>

        init {
            val (dc, ci, la) = prevState?.let {
                Triple(it.delayedCommands, it.commitIndex, it.lastApplied)
            } ?: Triple(emptyList<Command>().toMutableList(), 0, 0)
            delayedCommands = dc
            commitIndex = ci
            lastApplied = la
        }

        private val storage = env.storage
        fun lastLog(): LogId = storage.readLastLogId()
        fun logEntry(index: Int): LogEntry? = storage.readLog(index)
        fun currentTerm(): Int = storage.readPersistentState().currentTerm
        fun updateCurrentTerm(newTerm: Int) {
            if (newTerm != currentTerm()) storage.writePersistentState(PersistentState(newTerm, null))
        }

        fun setPersistentState(term: Int, votedFor: Int?) {
            val change = when (votedFor) {
                null -> votedFor() != null
                else -> if (votedFor() == null) true else votedFor != votedFor()
            }

            if (change || currentTerm() != term)
                storage.writePersistentState(PersistentState(term, votedFor))
        }

        fun setVotedFor(votedFor: Int?) {
            val change = when (votedFor) {
                null -> votedFor() != null
                else -> if (votedFor() == null) true else votedFor != votedFor()
            }
            if (change)
                storage.writePersistentState(PersistentState(currentTerm(), votedFor()))
        }

        fun votedFor(): Int? = storage.readPersistentState().votedFor

        fun addCommand(command: Command) {
            delayedCommands.add(command)
        }

    }

    private fun Message.react(srcId: Int) = when (this) {
        is AppendEntryRpc -> react()
        is RequestVoteRpc -> react()
        is RequestVoteResult -> react()
        is ClientCommandRpc -> react()
        is ClientCommandResult -> react()
        is AppendEntryResult -> react()
    }(srcId)

    fun AppendEntryRpc.react(): (Int) -> Unit
    fun AppendEntryResult.react(): (Int) -> Unit
    fun RequestVoteRpc.react(): (Int) -> Unit
    fun RequestVoteResult.react(): (Int) -> Unit
    fun ClientCommandRpc.react(): (Int) -> Unit
    fun ClientCommandResult.react(): (Int) -> Unit

    fun convertToFollowerAndReactOnMessage(srcId: Int, message: Message) =
        convertToFollower().also { externalProcess.onMessage(srcId, message) }

    fun convertToFollower() =
        externalProcess.changeCurrentStatus(Follower(currentState.env, currentState, externalProcess))

    fun sendMessage(destId: Int, message: Message) = currentState.env.send(destId, message)

    fun broadcast(message: Message) {
        (1 until currentState.env.nProcesses + 1).filter { it != currentState.env.processId }
            .forEach { sendMessage(it, message) }
    }

    fun applyCommand(command: Command, curTerm: Boolean) {
        val res = currentState.env.machine.apply(command)
        if (this is Leader && curTerm) {
            when (command.processId) {
                currentState.env.processId -> currentState.env.onClientCommandResult(res)
                else -> sendMessage(command.processId, ClientCommandResult(currentState.currentTerm(), res))
            }
        }
    }

    fun Message.defaultUpdateAndConvert(term: Int, srcId: Int, el: () -> Unit) {
        if (term > currentState.currentTerm()) {
            currentState.updateCurrentTerm(term)
            convertToFollowerAndReactOnMessage(srcId, this)
        } else el()
    }

}

class Follower(
    environment: Environment,
    prevState: AbstractProcess.CurrentState?,
    override val externalProcess: ProcessImpl
) : AbstractProcess {
    override val currentState: State = State(environment, prevState)

    override fun onCommand(command: Command) =
        currentState.leaderId?.let {
            sendMessage(it, ClientCommandRpc(currentState.currentTerm(), command))
        } ?: currentState.addCommand(command)

    override fun AppendEntryRpc.react(): (Int) -> Unit = {
        if (term < currentState.currentTerm()) {
            sendMessage(it, AppendEntryResult(currentState.currentTerm(), null))
        } else {


            if (currentState.currentTerm() < term) {
                currentState.updateCurrentTerm(term)
                currentState.setVotedFor(null)
            }

            if (term == currentState.currentTerm()) {
                setLeaderId(it, true)
            }

            currentState.env.startTimeout(Timeout.ELECTION_TIMEOUT)

            val prevLog = currentState.logEntry(prevLogId.index)

            var flag = false
            if (prevLogId != START_LOG_ID) {
                if (prevLog?.id?.term?.let { i -> i != prevLogId.term } != false) {
                    sendMessage(it, AppendEntryResult(currentState.currentTerm(), null))
                    flag = true
                }
            }

            if (!flag) {
                if (entry != null) {
                    currentState.env.storage.appendLogEntry(entry)

                    if (leaderCommit > currentState.commitIndex)
                        setCommitIndex(min(leaderCommit, entry.id.index))
                    else
                        applyC()

                    sendMessage(it, AppendEntryResult(currentState.currentTerm(), entry.id.index))
                } else {
                    if (leaderCommit > currentState.commitIndex) {
                        setCommitIndex(leaderCommit)
                    } else {
                        applyC()
                    }

                    sendMessage(
                        it, AppendEntryResult(
                            currentState.currentTerm(),
                            prevLog?.id?.index ?: START_LOG_ID.index
                        )
                    )
                }
            }
        }
    }

    private fun setCommitIndex(newCommitIndex: Int) {
        currentState.commitIndex = newCommitIndex
        applyC()
    }

    private fun applyC() {
        while (currentState.lastApplied < currentState.commitIndex) {
            currentState.logEntry(currentState.lastApplied + 1)?.let {
                currentState.lastApplied++
                applyCommand(it.command, false)
            } ?: break
        }
    }

    override fun AppendEntryResult.react(): (Int) -> Unit = {
        updateTerm(it, false)
    }

    override fun RequestVoteRpc.react(): (Int) -> Unit = {
        if (term < currentState.currentTerm()) {
            sendMessage(it, RequestVoteResult(currentState.currentTerm(), false))
        } else {
            val (newTerm, changed) =
                if (term > currentState.currentTerm()) Pair(term, true)
                else Pair(currentState.currentTerm(), false)

            val grant = (currentState.votedFor()?.let { v -> v == it } != false) && lastLogId >= currentState.lastLog()

            if (grant) {
                if (changed)
                    currentState.setPersistentState(newTerm, it)
                else
                    currentState.setVotedFor(it)
            } else if (changed) {
                currentState.updateCurrentTerm(newTerm)
            }

            sendMessage(it, RequestVoteResult(newTerm, grant))

            if (changed) convertToFollower()
        }
    }

    override fun RequestVoteResult.react(): (Int) -> Unit = {
        updateTerm(it, false)
    }

    override fun ClientCommandRpc.react(): (Int) -> Unit = {
        updateTerm(it, false)
        currentState.leaderId?.let { l -> sendMessage(l, ClientCommandRpc(currentState.currentTerm(), command)) }
            ?: currentState.addCommand(command)
    }

    override fun ClientCommandResult.react(): (Int) -> Unit = {
        updateTerm(it, true)
        currentState.env.onClientCommandResult(result)
    }

    private fun Message.updateTerm(srcId: Int, updateLeader: Boolean) {
        if (term >= currentState.currentTerm()) {
            val changeTerm = term > currentState.currentTerm()
            if (changeTerm) {
                currentState.updateCurrentTerm(term)
                currentState.setVotedFor(null)
            }
            if (updateLeader && currentState.leaderId == null) {
                setLeaderId(srcId, changeTerm)
            }
        }
    }

    private fun setLeaderId(leaderId: Int, cl: Boolean) {
        currentState.leaderId = leaderId
        currentState.delayedCommands.forEach { sendMessage(leaderId, ClientCommandRpc(currentState.currentTerm(), it)) }
        currentState.delayedCommands.clear()
        if (cl)
            currentState.env.startTimeout(Timeout.ELECTION_TIMEOUT)
    }

    class State(environment: Environment, prevState: AbstractProcess.CurrentState?) : AbstractProcess.CurrentState(
        environment, prevState
    ) {
        internal var leaderId: Int? = null
    }
}

class Leader(
    environment: Environment,
    prevState: AbstractProcess.CurrentState,
    override val externalProcess: ProcessImpl
) : AbstractProcess {
    override val currentState: State = State(environment, prevState)

    override fun onTimeout() {
        currentState.env.startTimeout(Timeout.LEADER_HEARTBEAT_PERIOD)
        broadcastHeartbeat()
    }

    override fun onCommand(command: Command) {
        currentState.env.storage.appendLogEntry(
            LogEntry(
                LogId(currentState.lastLog().index + 1, currentState.currentTerm()),
                command
            )
        )
        broadcastLastLogEntry()
    }

    override fun AppendEntryRpc.react(): (Int) -> Unit = {
        defaultUpdateAndConvert(term, it) {
            sendMessage(
                it,
                AppendEntryResult(currentState.currentTerm(), null)
            )
        }
    }

    override fun AppendEntryResult.react(): (Int) -> Unit = {
        defaultUpdateAndConvert(term, it)
        {
            if (lastIndex == null) {
                currentState.nextIndex[it - 1]--
                if (currentState.nextIndex[it - 1] <= currentState.matchIndex[it - 1])
                    currentState.nextIndex[it - 1] = currentState.matchIndex[it - 1] + 1
                sendMessage(it, newAppendEntryRpc(currentState.nextIndex[it - 1]))
            } else {
                currentState.nextIndex[it - 1] = lastIndex + 1
                currentState.setMatchIndex(it - 1, lastIndex)

                while (currentState.lastApplied < currentState.commitIndex) {
                    currentState.lastApplied++
                    val logEntry = currentState.logEntry(currentState.lastApplied)
                    logEntry?.let { l -> applyCommand(l.command, l.id.term == currentState.currentTerm()) }
                }

                if (currentState.nextIndex[it - 1] <= currentState.lastLog().index) {
                    sendMessage(it, newAppendEntryRpc(currentState.nextIndex[it - 1]))
                }
            }
        }
    }

    override fun RequestVoteRpc.react(): (Int) -> Unit = {
        defaultUpdateAndConvert(term, it) {
            sendMessage(
                it,
                RequestVoteResult(currentState.currentTerm(), false)
            )
        }
    }

    override fun RequestVoteResult.react(): (Int) -> Unit = {
        defaultUpdateAndConvert(term, it) { }
    }

    override fun ClientCommandRpc.react(): (Int) -> Unit = {
        defaultUpdateAndConvert(term, it) { onCommand(command) }
    }

    override fun ClientCommandResult.react(): (Int) -> Unit = {
        defaultUpdateAndConvert(term, it) { currentState.env.onClientCommandResult(result) }
    }

    private fun broadcastLastLogEntry() {
        val lastLog = currentState.lastLog()

        (1 until currentState.env.nProcesses + 1).filter { it != currentState.env.processId && currentState.nextIndex[it - 1] == lastLog.index }
            .forEach { sendMessage(it, newAppendEntryRpc(lastLog.index)) }
    }

    private fun newAppendEntryRpc(index: Int): AppendEntryRpc {
        val prevLogId = currentState.logEntry(index - 1)?.id ?: START_LOG_ID
        return AppendEntryRpc(
            currentState.currentTerm(),
            prevLogId,
            currentState.commitIndex,
            currentState.logEntry(index)
        )
    }

    fun broadcastHeartbeat() =
        broadcast(
            AppendEntryRpc(
                currentState.currentTerm(),
                currentState.lastLog(),
                currentState.commitIndex,
                null
            )
        )

    class State(environment: Environment, prevState: AbstractProcess.CurrentState) :
        AbstractProcess.CurrentState(environment, prevState) {

        val nextIndex: Array<Int> = Array(env.nProcesses) { lastLog().index + 1 }
        val matchIndex: Array<Int> = Array(env.nProcesses) { 0 }

        fun setMatchIndex(idx: Int, matchIdx: Int) {
            matchIndex[idx] = matchIdx
            matchIndex[env.processId - 1] = lastLog().index

            val matchIndexMap: NavigableMap<Int, Int> = TreeMap()

            (0 until env.nProcesses).forEach {
                matchIndexMap[matchIndex[it]] = matchIndexMap.getOrDefault(matchIndex[it], 0) + 1
            }

            for (n in matchIndexMap.lastKey() downTo commitIndex + 1) {
                if (logEntry(n)?.id?.term.let { it != currentTerm() }) continue
                if (matchIndexMap.tailMap(n, true).values.sum() > env.nProcesses / 2) {
                    commitIndex = n
                    break
                }
            }

        }
    }
}

class Candidate(
    environment: Environment,
    previousState: AbstractProcess.CurrentState,
    override val externalProcess: ProcessImpl,
) : AbstractProcess {

    override val currentState: State = State(environment, previousState)

    override fun onCommand(command: Command) = currentState.addCommand(command)

    override fun AppendEntryRpc.react(): (Int) -> Unit = {
        if (term >= currentState.currentTerm()) {
            currentState.updateCurrentTerm(term)
            convertToFollowerAndReactOnMessage(it, this)
        } else {
            sendMessage(it, AppendEntryResult(currentState.currentTerm(), null))
        }
    }

    override fun AppendEntryResult.react(): (Int) -> Unit = {
        defaultUpdateAndConvert(term, it) { }
    }

    override fun RequestVoteRpc.react(): (Int) -> Unit = {
        defaultUpdateAndConvert(term, it) {
            sendMessage(
                it,
                RequestVoteResult(currentState.currentTerm(), false)
            )
        }
    }

    override fun RequestVoteResult.react(): (Int) -> Unit = {
        defaultUpdateAndConvert(term, it) {
            if (voteGranted) {
                currentState.addVote(it)
                if (currentState.quorum())
                    externalProcess.changeCurrentStatus(Leader(currentState.env, currentState, externalProcess))
            }
        }
    }

    override fun ClientCommandRpc.react(): (Int) -> Unit = {
        defaultUpdateAndConvert(term, it) { currentState.addCommand(command) }
    }

    override fun ClientCommandResult.react(): (Int) -> Unit = {
        defaultUpdateAndConvert(term, it) { currentState.env.onClientCommandResult(result) }
    }

    companion object {
        class State(val environment: Environment, previousState: AbstractProcess.CurrentState) :
            AbstractProcess.CurrentState(environment, previousState) {

            private val votes: BooleanArray = BooleanArray(environment.nProcesses)

            fun addVote(srcId: Int) {
                votes[srcId - 1] = true
            }

            fun quorum(): Boolean =
                (0 until environment.nProcesses).count { votes[it] } > environment.nProcesses / 2
        }
    }

}