package raft.test

import org.junit.runner.*
import org.junit.runners.*
import raft.*
import raft.Message.*
import raft.Timeout.*
import raft.test.ProcessAction.*
import raft.test.ProcessAction.Result
import kotlin.random.*
import kotlin.test.*

sealed class ProcessAction {
    data class Send(val destId: Int, val message: Message) : ProcessAction()
    data class Result(val result: CommandResult) : ProcessAction()
    data class AppendLogEntry(val entry: LogEntry) : ProcessAction()
    data class ApplyCommand(val command: Command) : ProcessAction()
    data class WritePersistentState(val state: PersistentState) : ProcessAction()
    data class StartTimeout(val timeout: Timeout) : ProcessAction()
}

interface ActionSink {
    operator fun plusAssign(action: ProcessAction)
    fun removeActionIf(predicate: (ProcessAction) -> Boolean)
}

@RunWith(Parameterized::class)
class MockTest(
    override val processId: Int,
    override val nProcesses: Int,
    startTerm: Int,
    lastLogIndex: Int,
) : Environment, ActionSink {
    companion object {
        @JvmStatic
        @Parameterized.Parameters(name = "pid{0}/{1} term={2} log={3}")
        fun parameters() =
            listOf(
                arrayOf(1, 3, 0, 0),
                arrayOf(2, 3, 5, 2),
                arrayOf(3, 5, 7, 9),
                arrayOf(4, 7, 11, 15)
            )
    }

    private val rnd = Random(1)

    private val actions = ArrayList<ProcessAction>()

    override val storage = MockStorage(this, this, rnd, PersistentState(startTerm), lastLogIndex)
    override val machine = MockStateMachine(this)

    private val term: Int get() = storage.readPersistentState().currentTerm
    private val lastLogId: LogId get() = storage.readLastLogId()

    override operator fun plusAssign(action: ProcessAction) {
        actions += action
    }

    override fun removeActionIf(predicate: (ProcessAction) -> Boolean) {
        actions.removeIf(predicate)
    }

    override fun send(destId: Int, message: Message) {
        actions += Send(destId, message)
    }

    override fun startTimeout(timeout: Timeout) {
        removeActionIf { it is StartTimeout }
        actions += StartTimeout(timeout)
    }

    override fun onClientCommandResult(result: CommandResult) {
        actions += Result(result)
    }

    private val process = ProcessImpl(this)

    @BeforeTest
    fun initFollower() {
        expectActions(StartTimeout(ELECTION_TIMEOUT))
    }

    @AfterTest
    fun checkNoMoreActions() {
        expectActions()
    }

    @Test
    fun `FOLLOWER responds on ping in the current term`() {
        val leaderId = (processId + rnd.nextInt(nProcesses - 1)) % nProcesses + 1
        process.onMessage(leaderId, AppendEntryRpc(term, lastLogId, 0, null))
        expectActions(
            Send(leaderId, AppendEntryResult(term, lastLogId.index)),
            StartTimeout(ELECTION_TIMEOUT)
        )
    }

    @Test
    fun `FOLLOWER responds on ping in the new term`() {
        val leaderId = (processId + rnd.nextInt(nProcesses - 1)) % nProcesses + 1
        val newTerm = term + rnd.nextInt(1..3)
        process.onMessage(leaderId, AppendEntryRpc(newTerm, lastLogId, 0, null))
        expectActions(
            Send(leaderId, AppendEntryResult(newTerm, lastLogId.index)),
            WritePersistentState(PersistentState(newTerm)),
            StartTimeout(ELECTION_TIMEOUT)
        )
    }

    @Test
    fun `FOLLOWER votes for a candidate with the same lastLogId`() {
        val leaderId = (processId + rnd.nextInt(nProcesses - 1)) % nProcesses + 1
        val newTerm = term + rnd.nextInt(1..3)
        // Updates term and votes once
        process.onMessage(leaderId, RequestVoteRpc(newTerm, lastLogId))
        expectActions(
            Send(leaderId, RequestVoteResult(newTerm, true)),
            WritePersistentState(PersistentState(newTerm, votedFor = leaderId)),
            StartTimeout(ELECTION_TIMEOUT)
        )
        // Votes again for the same candidate
        process.onMessage(leaderId, RequestVoteRpc(newTerm, lastLogId))
        expectActions(
            Send(leaderId, RequestVoteResult(newTerm, true)),
        )
    }

    @Test
    fun `FOLLOWER votes for a candidate with more up-to-date log (new term, old index)`() {
        if (term <= 1 || lastLogId.index == 0) return
        val oldIndex = findOldLogId().index
        val candidateId = (processId + rnd.nextInt(nProcesses - 1)) % nProcesses + 1
        val newTerm = term + rnd.nextInt(1..3)
        val oldLogId = LogId(oldIndex, newTerm)
        process.onMessage(candidateId, RequestVoteRpc(newTerm, oldLogId))
        expectActions(
            WritePersistentState(PersistentState(newTerm, votedFor = candidateId)),
            Send(candidateId, RequestVoteResult(newTerm, true)),
            StartTimeout(ELECTION_TIMEOUT)
        )
    }

    @Test
    fun `FOLLOWER refuses to vote for a different candidate`() {
        val candidateId1 = processId % nProcesses + 1
        val candidateId2 = candidateId1 % nProcesses + 1
        val newTerm = term + rnd.nextInt(1..3)
        // Updates term and grants vote to leaderId1
        process.onMessage(candidateId1, RequestVoteRpc(newTerm, lastLogId))
        expectActions(
            Send(candidateId1, RequestVoteResult(newTerm, true)),
            WritePersistentState(PersistentState(newTerm, votedFor = candidateId1)),
            StartTimeout(ELECTION_TIMEOUT)
        )
        // Refuses vote to leaderId2
        process.onMessage(candidateId2, RequestVoteRpc(newTerm, lastLogId))
        expectActions(
            Send(candidateId2, RequestVoteResult(newTerm, false)),
        )
    }

    @Test
    fun `FOLLOWER refuses to vote for a candidate with not up-to-date log (old index)`() {
        if (lastLogId.index <= 0) return
        val candidateId = (processId + rnd.nextInt(nProcesses - 1)) % nProcesses + 1
        val newTerm = term + rnd.nextInt(1..3)
        val oldLogId = storage.readLog(rnd.nextInt(0 until lastLogId.index))?.id ?: START_LOG_ID
        process.onMessage(candidateId, RequestVoteRpc(newTerm, oldLogId))
        expectActions(
            WritePersistentState(PersistentState(newTerm)),
            Send(candidateId, RequestVoteResult(newTerm, false)),
            StartTimeout(ELECTION_TIMEOUT)
        )
    }

    @Test
    fun `FOLLOWER refuses to vote for a candidate with not up-to-date log (old term)`() {
        if (lastLogId.term <= 1) return
        val candidateId = (processId + rnd.nextInt(nProcesses - 1)) % nProcesses + 1
        val newTerm = term + rnd.nextInt(1..3)
        val oldLogId = LogId(lastLogId.index, lastLogId.term - 1)
        process.onMessage(candidateId, RequestVoteRpc(newTerm, oldLogId))
        expectActions(
            WritePersistentState(PersistentState(newTerm)),
            Send(candidateId, RequestVoteResult(newTerm, false)),
            StartTimeout(ELECTION_TIMEOUT)
        )
    }

    @Test
    fun `FOLLOWER refuses to vote for a candidate with not up-to-date log (new index, old term)`() {
        if (lastLogId.term <= 1) return
        val candidateId = (processId + rnd.nextInt(nProcesses - 1)) % nProcesses + 1
        val newTerm = term + rnd.nextInt(1..3)
        val newIndex = lastLogId.index + rnd.nextInt(1..3)
        val oldLogId = LogId(newIndex, lastLogId.term - 1)
        process.onMessage(candidateId, RequestVoteRpc(newTerm, oldLogId))
        expectActions(
            WritePersistentState(PersistentState(newTerm)),
            Send(candidateId, RequestVoteResult(newTerm, false)),
            StartTimeout(ELECTION_TIMEOUT)
        )
    }

    @Test
    fun `FOLLOWER refuses to vote for a stale candidate`() {
        if (term == 0) return
        val candidateId = (processId + rnd.nextInt(nProcesses - 1)) % nProcesses + 1
        val oldTerm = rnd.nextInt(0 until term)
        process.onMessage(candidateId, RequestVoteRpc(oldTerm, lastLogId))
        expectActions(
            Send(candidateId, RequestVoteResult(term, false))
        )
    }

    @Test
    fun `FOLLOWER appends matching log entries`() {
        val leaderId = (processId + rnd.nextInt(nProcesses - 1)) % nProcesses + 1
        val newTerm = term + rnd.nextInt(1..3)
        // Append one entry
        val entry1 = rnd.nextLogEntry(lastLogId.index + 1, newTerm, this)
        process.onMessage(leaderId, AppendEntryRpc(newTerm, lastLogId, 0, entry1))
        expectActions(
            Send(leaderId, AppendEntryResult(newTerm, entry1.id.index)),
            WritePersistentState(PersistentState(newTerm)),
            AppendLogEntry(entry1),
            StartTimeout(ELECTION_TIMEOUT)
        )
        // Append another entry from the same term
        val entry2 = rnd.nextLogEntry(lastLogId.index + 1, newTerm, this)
        process.onMessage(leaderId, AppendEntryRpc(newTerm, entry1.id, 0, entry2))
        expectActions(
            Send(leaderId, AppendEntryResult(newTerm, entry2.id.index)),
            AppendLogEntry(entry2),
            StartTimeout(ELECTION_TIMEOUT)
        )
    }

    @Test
    fun `FOLLOWER rejects unmatched log entry`() {
        val leaderId = (processId + rnd.nextInt(nProcesses - 1)) % nProcesses + 1
        val newTerm = term + rnd.nextInt(1..3)
        val entry = rnd.nextLogEntry(lastLogId.index + 1, newTerm, this)
        process.onMessage(leaderId, AppendEntryRpc(newTerm, entry.id, 0, entry))
        expectActions(
            Send(leaderId, AppendEntryResult(newTerm, null)),
            WritePersistentState(PersistentState(newTerm)),
            StartTimeout(ELECTION_TIMEOUT)
        )
    }

    @Test
    fun `FOLLOWER commits all log entries on leader commit`() {
        val leaderId = (processId + rnd.nextInt(nProcesses - 1)) % nProcesses + 1
        val lastLogIndex = lastLogId.index
        process.onMessage(leaderId, AppendEntryRpc(term, lastLogId, lastLogIndex, null))
        expectActions(
            (1..lastLogIndex).map { ApplyCommand(storage.readLog(it)!!.command) },
            Send(leaderId, AppendEntryResult(term, lastLogIndex)),
            StartTimeout(ELECTION_TIMEOUT)
        )
    }

    @Test
    fun `FOLLOWER commits some log entries on leader commit`() {
        val leaderId = (processId + rnd.nextInt(nProcesses - 1)) % nProcesses + 1
        val lastLogIndex = lastLogId.index
        val leaderCommit = rnd.nextInt(0..lastLogIndex)
        process.onMessage(leaderId, AppendEntryRpc(term, lastLogId, leaderCommit, null))
        expectActions(
            (1..leaderCommit).map { ApplyCommand(storage.readLog(it)!!.command) },
            Send(leaderId, AppendEntryResult(term, lastLogIndex)),
            StartTimeout(ELECTION_TIMEOUT)
        )
    }

    @Test
    fun `FOLLOWER rejects commit message from stale leader`() {
        if (term == 0) return
        val leaderId = (processId + rnd.nextInt(nProcesses - 1)) % nProcesses + 1
        val oldTerm = rnd.nextInt(0 until term)
        process.onMessage(leaderId, AppendEntryRpc(oldTerm, lastLogId, lastLogId.index, null))
        expectActions(
            Send(leaderId, AppendEntryResult(term, null))
        )
    }

    @Test
    fun `FOLLOWER queues direct client command and forwards it to leader on ping`() {
        val leaderId = (processId + rnd.nextInt(nProcesses - 1)) % nProcesses + 1
        // the first command
        val command1 = rnd.nextCommand(processId)
        process.onClientCommand(command1)
        expectActions() // nothing
        process.onMessage(leaderId, AppendEntryRpc(term, lastLogId, 0, null))
        expectActions(
            Send(leaderId, AppendEntryResult(term, lastLogId.index)),
            Send(leaderId, ClientCommandRpc(term, command1)),
            StartTimeout(ELECTION_TIMEOUT)
        )
        // one more command
        val command2 = rnd.nextCommand(processId)
        process.onClientCommand(command2)
        expectActions(
            Send(leaderId, ClientCommandRpc(term, command2))
        )
    }

    @Test
    fun `FOLLOWER queues forwarded client commands and forwards it to leader on ping`() {
        if (term <= 1) return
        val oldTerm = rnd.nextInt(1..term - 1)
        val clientId = (processId + rnd.nextInt(nProcesses - 1)) % nProcesses + 1
        val leaderId = (processId + rnd.nextInt(nProcesses - 1)) % nProcesses + 1
        // the first command
        val command1 = rnd.nextCommand(clientId)
        process.onMessage(clientId, ClientCommandRpc(oldTerm, command1))
        expectActions() // nothing
        process.onMessage(leaderId, AppendEntryRpc(term, lastLogId, 0, null))
        expectActions(
            Send(leaderId, AppendEntryResult(term, lastLogId.index)),
            Send(leaderId, ClientCommandRpc(term, command1)),
            StartTimeout(ELECTION_TIMEOUT)
        )
        // one more stable command from old term
        val command2 = rnd.nextCommand(clientId)
        process.onMessage(clientId, ClientCommandRpc(oldTerm, command2))
        expectActions(
            Send(leaderId, ClientCommandRpc(term, command2))
        )
    }

    @Test
    fun `FOLLOWER reports command result`() {
        val leaderId = (processId + rnd.nextInt(nProcesses - 1)) % nProcesses + 1
        val result = rnd.nextCommandResult()
        process.onMessage(leaderId, ClientCommandResult(term, result))
        expectActions(
            Result(result)
        )
    }

    @Test
    fun `FOLLOWER learns new term leader on command result and forwards its queue`() {
        // command to the follow who does not know the leader yet
        val myCommand = rnd.nextCommand(processId)
        process.onClientCommand(myCommand)
        // message from some new leader
        val leaderId = (processId + rnd.nextInt(nProcesses - 1)) % nProcesses + 1
        val newTerm = term + rnd.nextInt(1..3)
        val result = rnd.nextCommandResult()
        process.onMessage(leaderId, ClientCommandResult(newTerm, result))
        expectActions(
            Result(result),
            WritePersistentState(PersistentState(newTerm)),
            StartTimeout(ELECTION_TIMEOUT), // must restart waiting heartbeats from the new leader
            Send(leaderId, ClientCommandRpc(newTerm, myCommand)) // forwards its client command to the new leader
        )
    }

    @Test
    fun `FOLLOWER replies 'no success' to AppendEntryRpc from an old term`() {
        if (term <= 1 || lastLogId.index == 0) return
        val oldLogId = findOldLogId()
        val oldTerm = oldLogId.term
        val oldIndex = oldLogId.index
        val oldLeaderId = (processId + rnd.nextInt(nProcesses - 1)) % nProcesses + 1
        val prevLogId = storage.readLog(oldIndex - 1)?.id ?: START_LOG_ID
        val oldEntry = rnd.nextLogEntry(oldIndex, oldTerm, this)
        process.onMessage(oldLeaderId, AppendEntryRpc(oldTerm, prevLogId, oldIndex, oldEntry))
        expectActions(
            Send(oldLeaderId, AppendEntryResult(term, null))
        )
    }

    @Test
    fun `FOLLOWER ignores AppendEntryResult from an old term`() {
        if (term <= 1) return
        val oldTerm = rnd.nextInt(1 until term)
        val followerId = (processId + rnd.nextInt(nProcesses - 1)) % nProcesses + 1
        process.onMessage(followerId, AppendEntryResult(oldTerm, null))
        expectActions()
        process.onMessage(followerId, AppendEntryResult(oldTerm, 1))
        expectActions()
    }

    @Test
    fun `FOLLOWER ignores RequestVoteResult from an old term`() {
        if (term <= 1 || lastLogId.index == 0) return
        val oldLogId = findOldLogId()
        val oldTerm = oldLogId.term
        val candidateId = (processId + rnd.nextInt(nProcesses - 1)) % nProcesses + 1
        process.onMessage(candidateId, RequestVoteResult(oldTerm, true))
        expectActions()
    }

    private fun initCandidate() {
        val nextTerm = term + 1
        process.onTimeout()
        expectCandidateVoteRequest(nextTerm)
    }

    private fun expectCandidateVoteRequest(term: Int) {
        expectActions(
            (1..nProcesses).filter { it != processId }.map {
                Send(it, RequestVoteRpc(term, lastLogId))
            },
            WritePersistentState(PersistentState(term, processId)),
            StartTimeout(ELECTION_TIMEOUT)
        )
    }

    @Test
    fun `CANDIDATE requests votes on timeouts`() {
        initCandidate()
        initCandidate()
    }

    @Test
    fun `CANDIDATE does not become a leader without enough votes`() {
        initCandidate()
        val otherIds = (1..nProcesses).filter { it != processId }
        val grantedVote = otherIds.shuffled(rnd).take(nProcesses / 2 - 1).toSet()
        for (id in otherIds.shuffled(rnd)) {
            process.onMessage(id, RequestVoteResult(term, id in grantedVote))
            expectActions()
        }
    }

    @Test
    fun `CANDIDATE refuses to vote for another one in the same term`() {
        initCandidate()
        val leaderId = (processId + rnd.nextInt(nProcesses - 1)) % nProcesses + 1
        process.onMessage(leaderId, RequestVoteRpc(term, lastLogId))
        expectActions(
            Send(leaderId, RequestVoteResult(term, false))
        )
    }

    @Test
    fun `CANDIDATE refuses to vote for a stale candidate`() {
        if (term == 0) return
        val candidateId = (processId + rnd.nextInt(nProcesses - 1)) % nProcesses + 1
        val oldTerm = rnd.nextInt(0 until term)
        process.onMessage(candidateId, RequestVoteRpc(oldTerm, lastLogId))
        expectActions(
            Send(candidateId, RequestVoteResult(term, false))
        )
    }

    @Test
    fun `CANDIDATE queues client commands and forwards them to leader on ping`() {
        // the first command
        val command1 = rnd.nextCommand(processId)
        process.onClientCommand(command1)
        expectActions() // nothing
        // becomes candidate
        initCandidate()
        // the second command
        val command2 = rnd.nextCommand(processId)
        process.onClientCommand(command2)
        expectActions() // nothing
        // ping from the leader
        val leaderId = (processId + rnd.nextInt(nProcesses - 1)) % nProcesses + 1
        process.onMessage(leaderId, AppendEntryRpc(term, lastLogId, 0, null))
        expectActions(
            Send(leaderId, AppendEntryResult(term, lastLogId.index)),
            Send(leaderId, ClientCommandRpc(term, command1)),
            Send(leaderId, ClientCommandRpc(term, command2)),
            StartTimeout(ELECTION_TIMEOUT)
        )
        // one more command
        val command3 = rnd.nextCommand(processId)
        process.onClientCommand(command3)
        expectActions(
            Send(leaderId, ClientCommandRpc(term, command3))
        )
    }

    @Test
    fun `CANDIDATE reports client command result from old term when it was a follower`() {
        if (term <= 1) return
        val oldTerm = term - 1
        val oldLeaderId = (processId + rnd.nextInt(nProcesses - 1)) % nProcesses + 1
        initCandidate()
        val result = rnd.nextCommandResult()
        process.onMessage(oldLeaderId, ClientCommandResult(oldTerm, result))
        expectActions(
            Result(result)
        )
    }

    @Test
    fun `CANDIDATE replies 'no success' to AppendEntryRpc from an old term`() {
        if (term <= 1 || lastLogId.index == 0) return
        val oldLogId = findOldLogId()
        val oldTerm = oldLogId.term
        val oldIndex = oldLogId.index
        val oldLeaderId = (processId + rnd.nextInt(nProcesses - 1)) % nProcesses + 1
        val prevLogId = storage.readLog(oldIndex - 1)?.id ?: START_LOG_ID
        val oldEntry = rnd.nextLogEntry(oldIndex, oldTerm, this)
        initCandidate()
        process.onMessage(oldLeaderId, AppendEntryRpc(oldTerm, prevLogId, oldIndex, oldEntry))
        expectActions(
            Send(oldLeaderId, AppendEntryResult(term, null))
        )
    }

    @Test
    fun `CANDIDATE ignores AppendEntryResult from an old term`() {
        if (term <= 1) return
        val oldTerm = rnd.nextInt(1 until term)
        val followerId = (processId + rnd.nextInt(nProcesses - 1)) % nProcesses + 1
        initCandidate()
        process.onMessage(followerId, AppendEntryResult(oldTerm, null))
        expectActions()
        process.onMessage(followerId, AppendEntryResult(oldTerm, 1))
        expectActions()
    }

    @Test
    fun `CANDIDATE gathers enough votes and becomes leader`() {
        initLeader()
    }

    private fun initLeader() {
        initCandidate()
        val ids = (1..nProcesses).filter { it != processId }.shuffled(rnd).take(nProcesses / 2)
        for (id in ids) {
            expectActions() // while not last
            process.onMessage(id, RequestVoteResult(term, true))
        }
        // became leader, sends heartbeats
        expectHeartbeats()
    }

    private fun expectHeartbeats() {
        expectActions(
            (1..nProcesses).filter { it != processId }.map {
                Send(it, AppendEntryRpc(term, lastLogId, 0, null))
            },
            StartTimeout(LEADER_HEARTBEAT_PERIOD)
        )
    }

    @Test
    fun `LEADER sends heartbeats on timeout`() {
        initLeader()
        process.onTimeout()
        expectHeartbeats()
    }

    @Test
    fun `LEADER receivers ping and becomes follower`() {
        initLeader()
        val leaderId = (processId + rnd.nextInt(nProcesses - 1)) % nProcesses + 1
        val newTerm = term + rnd.nextInt(1..3)
        process.onMessage(leaderId, AppendEntryRpc(newTerm, lastLogId, 0, null))
        expectActions(
            Send(leaderId, AppendEntryResult(newTerm, lastLogId.index)),
            WritePersistentState(PersistentState(newTerm)),
            StartTimeout(ELECTION_TIMEOUT)
        )
    }

    @Test
    fun `LEADER receivers entry and becomes follower`() {
        initLeader()
        val leaderId = (processId + rnd.nextInt(nProcesses - 1)) % nProcesses + 1
        val newTerm = term + rnd.nextInt(1..3)
        // Append one entry
        val entry1 = rnd.nextLogEntry(lastLogId.index + 1, newTerm, this)
        process.onMessage(leaderId, AppendEntryRpc(newTerm, lastLogId, 0, entry1))
        expectActions(
            Send(leaderId, AppendEntryResult(newTerm, entry1.id.index)),
            WritePersistentState(PersistentState(newTerm)),
            AppendLogEntry(entry1),
            StartTimeout(ELECTION_TIMEOUT)
        )
        // Append another entry from the same term
        val entry2 = rnd.nextLogEntry(lastLogId.index + 1, newTerm, this)
        process.onMessage(leaderId, AppendEntryRpc(newTerm, entry1.id, 0, entry2))
        expectActions(
            Send(leaderId, AppendEntryResult(newTerm, entry2.id.index)),
            AppendLogEntry(entry2),
            StartTimeout(ELECTION_TIMEOUT)
        )
    }

    @Test
    fun `LEADER applies direct client commands`() {
        initLeader()
        // the first command
        val command1 = rnd.nextCommand(processId)
        val lastLogId1 = lastLogId
        val entry1 = LogEntry(LogId(lastLogId1.index + 1, term), command1)
        process.onClientCommand(command1)
        expectActions(
            (1..nProcesses).filter { it != processId }.map {
                Send(it, AppendEntryRpc(term, lastLogId1, 0, entry1))
            },
            AppendLogEntry(entry1)
        )
        // subsequent command will not generate AppendEntryRpc (until they've responded to)
        repeat(2) {
            val command2 = rnd.nextCommand(processId)
            val lastLogId2 = lastLogId
            val entry2 = LogEntry(LogId(lastLogId2.index + 1, term), command2)
            process.onClientCommand(command2)
            expectActions(
                AppendLogEntry(entry2)
            )
        }
    }

    @Test
    fun `LEADER applies forwarded client commands`() {
        initLeader()
        // the first command
        val clientId1 = (processId + rnd.nextInt(nProcesses - 1)) % nProcesses + 1
        val command1 = rnd.nextCommand(clientId1)
        val lastLogId1 = lastLogId
        val entry1 = LogEntry(LogId(lastLogId1.index + 1, term), command1)
        process.onMessage(clientId1, ClientCommandRpc(term, command1))
        expectActions(
            (1..nProcesses).filter { it != processId }.map {
                Send(it, AppendEntryRpc(term, lastLogId1, 0, entry1))
            },
            AppendLogEntry(entry1)
        )
        // subsequent command will not generate AppendEntryRpc (until they've responded to)
        repeat(2) {
            val clientId2 = (processId + rnd.nextInt(nProcesses - 1)) % nProcesses + 1
            val command2 = rnd.nextCommand(clientId2)
            val lastLogId2 = lastLogId
            val entry2 = LogEntry(LogId(lastLogId2.index + 1, term), command2)
            process.onMessage(clientId2, ClientCommandRpc(term, command2))
            expectActions(
                AppendLogEntry(entry2)
            )
        }
    }

    @Test
    fun `LEADER scans back log for on mismatched follower`() {
        initLeader()
        val followerId = (processId + rnd.nextInt(nProcesses - 1)) % nProcesses + 1
        var lasIndex = lastLogId.index
        repeat(3) {
            if (lasIndex == 0) return
            process.onMessage(followerId, AppendEntryResult(term, null))
            expectActions(
                Send(followerId, AppendEntryRpc(term,
                    prevLogId = storage.readLog(lasIndex - 1)?.id ?: START_LOG_ID,
                    leaderCommit = 0,
                    entry = storage.readLog(lasIndex)
                ))
            )
            lasIndex--
        }
    }

    private fun initLeaderWithNextIndices() {
        initLeader()
        // Ok response on leader's pings from all processes
        for (id in 1..nProcesses) if (id != processId) {
            process.onMessage(id, AppendEntryResult(term, lastLogId.index))
            expectActions()
        }
    }

    @Test
    fun `LEADER commits direct client commands`() {
        initLeaderWithNextIndices()
        // process commands
        var leaderCommit = 0
        val expectedMachine = StateMachine()
        for (i in 1..lastLogId.index) expectedMachine.apply(storage.readLog(i)!!.command)
        repeat(3) {
            val command = rnd.nextCommand(processId)
            val lastLogId = lastLogId
            val entry = LogEntry(LogId(lastLogId.index + 1, term), command)
            process.onClientCommand(command)
            expectActions(
                (1..nProcesses).filter { it != processId }.map {
                    Send(it, AppendEntryRpc(term, lastLogId, leaderCommit, entry))
                },
                AppendLogEntry(entry)
            )
            // responses in random order
            val ids = (1..nProcesses).filter { it != processId }.shuffled(rnd)
            var count = 0
            for (id in ids) {
                process.onMessage(id, AppendEntryResult(term, entry.id.index))
                if (++count == nProcesses / 2) { // commit on majority of answers
                    val result = expectedMachine.apply(command)
                    expectActions(
                        (leaderCommit + 1..entry.id.index).map {
                            ApplyCommand(storage.readLog(it)!!.command)
                        },
                        Result(result)
                    )
                } else {
                    expectActions() // nothin special otherwise
                }
            }
            leaderCommit = entry.id.index
        }
    }

    @Test
    fun `LEADER commits forwarded client commands`() {
        initLeaderWithNextIndices()
        // process commands
        var leaderCommit = 0
        val expectedMachine = StateMachine()
        for (i in 1..lastLogId.index) expectedMachine.apply(storage.readLog(i)!!.command)
        repeat(3) {
            val clientId = (processId + rnd.nextInt(nProcesses - 1)) % nProcesses + 1
            val command = rnd.nextCommand(clientId)
            val lastLogId = lastLogId
            val entry = LogEntry(LogId(lastLogId.index + 1, term), command)
            process.onMessage(clientId, ClientCommandRpc(term, command))
            expectActions(
                (1..nProcesses).filter { it != processId }.map {
                    Send(it, AppendEntryRpc(term, lastLogId, leaderCommit, entry))
                },
                AppendLogEntry(entry)
            )
            // responses in random order
            val ids = (1..nProcesses).filter { it != processId }.shuffled(rnd)
            var count = 0
            for (id in ids) {
                process.onMessage(id, AppendEntryResult(term, entry.id.index))
                if (++count == nProcesses / 2) { // commit on majority of answers
                    val result = expectedMachine.apply(command)
                    expectActions(
                        (leaderCommit + 1..entry.id.index).map {
                            ApplyCommand(storage.readLog(it)!!.command)
                        },
                        Send(clientId, ClientCommandResult(term, result))
                    )
                } else {
                    expectActions() // nothing special otherwise
                }
            }
            leaderCommit = entry.id.index
        }
    }

    @Test
    fun `LEADER reports client command result from old term when it was a follower`() {
        if (term <= 1) return
        val oldTerm = term - 1
        val oldLeaderId = (processId + rnd.nextInt(nProcesses - 1)) % nProcesses + 1
        initLeader()
        val result = rnd.nextCommandResult()
        process.onMessage(oldLeaderId, ClientCommandResult(oldTerm, result))
        expectActions(
            Result(result)
        )
    }

    @Test
    fun `LEADER replies 'no success' to AppendEntryRpc from an old term`() {
        if (term <= 1 || lastLogId.index == 0) return
        val oldLogId = findOldLogId()
        val oldTerm = oldLogId.term
        val oldIndex = oldLogId.index
        val oldLeaderId = (processId + rnd.nextInt(nProcesses - 1)) % nProcesses + 1
        val prevLogId = storage.readLog(oldIndex - 1)?.id ?: START_LOG_ID
        val oldEntry = rnd.nextLogEntry(oldIndex, oldTerm, this)
        initLeader()
        process.onMessage(oldLeaderId, AppendEntryRpc(oldTerm, prevLogId, oldIndex, oldEntry))
        expectActions(
            Send(oldLeaderId, AppendEntryResult(term, null))
        )
    }

    private fun findOldLogId(): LogId {
        while (true) {
            val id = storage.readLog(rnd.nextInt(1..lastLogId.index))!!.id
            if (id.term < term) return id
        }
    }

    @Test
    fun `LEADER refuses to vote for a stale candidate`() {
        if (term == 0) return
        val candidateId = (processId + rnd.nextInt(nProcesses - 1)) % nProcesses + 1
        val oldTerm = rnd.nextInt(0 until term)
        initLeader()
        process.onMessage(candidateId, RequestVoteRpc(oldTerm, lastLogId))
        expectActions(
            Send(candidateId, RequestVoteResult(term, false))
        )
    }

    private fun expectActions(expected: List<ProcessAction>, vararg more: ProcessAction) =
        expectActions(*expected.toTypedArray(), *more)

    private fun expectActions(vararg expected: ProcessAction) {
        for (action in expected) {
            assert(actions.remove(action)) {
                "Expected action: $action, but was only:\n${actions.joinToString("\n") { "\t\t$it" }}"
            }
        }
        assert(actions.isEmpty()) {
            "Expected no other actions:\n${actions.joinToString("\n") { "\t\t$it" }}"
        }
        actions.clear()
    }
}