package raft

import kotlinx.serialization.*
import raft.system.*
import java.util.*

/** Command performed by the client on [StateMachine]. */
@Serializable
data class Command(
    /** Process that originally issued a command is expecting to get the result. */
    val processId: Int,
    /** Identifier of the command. */
    val commandId: Int,
    /** Key. */
    val key: String,
    /** Value to set for the [key]. */
    val value: String
) {
    fun toNodeCommand(): String = "set #$commandId# $key $value"
}

/** Result of the [Command]. */
@Serializable
data class CommandResult(
    /** Identifier of the original command, equal to [Command.commandId]. */
    val commandId: Int,
    /** Key of the original command, equal to [Command.key]. */
    val key: String,
    /** Old value of the key before the execution of the command. */
    val oldValue: String?
)

/** State machine. */
open class StateMachine(map: Map<String, String> = emptyMap()) {
    private val map: MutableMap<String, String> = TreeMap(map)

    /** Applies command to the state machine and returns the result. */
    open fun apply(command: Command): CommandResult =
        CommandResult(command.commandId, command.key, map.put(command.key, command.value))

    constructor(dump: String) : this(json.decodeFromString<Map<String, String>>(dump))
    override fun toString() = json.encodeToString(map)
    override fun equals(other: Any?): Boolean = other is StateMachine && other.map == map
    override fun hashCode(): Int = map.hashCode()
}