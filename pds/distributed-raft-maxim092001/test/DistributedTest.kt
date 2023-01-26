package raft.test

import raft.*
import raft.system.*
import kotlin.random.*
import kotlin.test.*

class DistributedTest {
    private val sys = DistributedTestSystem(System.getProperty("implName") ?: DEFAULT_IMPL_NAME)
    private val nProcesses = Configuration.nodes.size
    private val rnd = Random(1)
    private val expectedMachine = StateMachine()
    private var lastCommandId = 100

    @BeforeTest
    fun setup() {
        sys.awaitListening()
    }

    @AfterTest
    fun tearDown() {
        sys.reqExit()
        sys.awaitTermination()
        sys.checkNotFailed()
    }

    private fun checkDumpsAtTheEnd() {
        sys.checkNotFailed()
        // execute a command to commit all state machines
        val command = Command(1, 0, "0", "0")
        val expectedResult = expectedMachine.apply(command)
        sys.request(1, command.toNodeCommand())
        val result = sys.awaitClientCommandResult()
        assertEquals(expectedResult, result.result)
        // wait until all processes have commited this command
        for (id in 1..nProcesses) {
            do {
                val lastCommitted = sys.awaitCommit(id)
            } while (lastCommitted != command)
        }
        // now check dumps
        sys.reqAll("dump")
        for (id in 1..nProcesses) {
            val machine = sys.awaitDump(id)
            assertEquals(expectedMachine, machine, "State machine of process $id")
        }
    }

    /**
     * A basic test, no restarts.
     */
    @Test
    fun testCommandsOneByOne() {
        repeat(100) {
            performRandomCommandsAndAwait(1)
        }
        checkDumpsAtTheEnd()
    }

    /**
     * A complicated test with restarts and command batches.
     */
    @Test
    fun testCommandsRestartsInBetween() {
        repeat(50) {
            // usually restart one process, but sometimes a random number
            val nRestarts = if (rnd.nextInt(4) == 0) rnd.nextInt(0..nProcesses) else 1
            val pids = (1..nProcesses).shuffled(rnd).take(nRestarts)
            for (pid in pids) sys.request(pid, "restart")
            for (pid in pids) sys.awaitRestart(pid)
            performRandomCommandsAndAwait(rnd.nextInt(1..3))
        }
        checkDumpsAtTheEnd()
    }

    private fun performRandomCommandsAndAwait(nCommands: Int) {
        val expectedProcessId = rnd.nextInt(1..nProcesses)
        val commands = List(nCommands) {
            rnd.nextCommand(expectedProcessId, ++lastCommandId)
        }
        val expectedResults = commands.map { expectedMachine.apply(it) }.toMutableList()
        for (command in commands) {
            sys.request(command.processId, command.toNodeCommand())
        }
        while(expectedResults.isNotEmpty()) {
            val (processId, result) = sys.awaitClientCommandResult()
            assertEquals(expectedProcessId, processId)
            val expectedResult = expectedResults.find { it.commandId == result.commandId }
            assertEquals(expectedResult, result)
            expectedResults.remove(expectedResult)
        }
    }
}