package raft.test

import raft.*
import kotlin.random.*

private val randomStrings = run {
    val rnd = Random(1)
    List(10) {
        buildString {
            repeat(rnd.nextInt(1..3)) {
                append('A' + rnd.nextInt(26))
            }
        }
    }
}

fun Random.nextString() = randomStrings.random(this)

fun Random.nextCommand(processId: Int, commandId: Int = nextCommandId()) =
    Command(processId, commandId, nextString(), nextString())

fun Random.nextCommandResult() =
    CommandResult(nextCommandId(), nextString(), nextString())

fun Random.nextCommandId() = nextInt(10..99)

fun Random.nextLogEntry(index: Int, term: Int, env: Environment) =
    LogEntry(LogId(index, term), nextCommand(nextInt(1..env.nProcesses)))