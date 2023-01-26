package mutex

import mutex.MessageFieldType.*
import java.io.*

/**
 * Inter-process communication message.
 */
class Message(val bytes: ByteArray) {
    override fun toString(): String = parse {
        generateSequence {
            when (cur) {
                END -> null
                INT -> readInt().toString()
                STR -> readString()
                ENUM -> readEnumName()
            }
        }.joinToString(separator = ", ")
    }
}

/**
 * Builder class for env messages.
 */
class MessageBuilder {
    private val out = ByteArrayOutputStream()
    private val data = DataOutputStream(out)

    fun writeInt(value: Int) {
        writeField(INT)
        data.writeInt(value)
    }

    fun writeString(str: String) {
        writeField(STR)
        data.writeUTF(str)
    }

    fun writeEnum(enum: Enum<*>) {
        writeField(ENUM)
        data.writeUTF(enum.name)
    }

    fun build(): Message = Message(out.toByteArray())

    private fun writeField(t: MessageFieldType) {
        data.writeByte(t.ordinal)
    }
}

/**
 * Parser class for messages.
 */
class MessageParser(message: Message) {
    private val data = DataInputStream(ByteArrayInputStream(message.bytes))
    private var curField: MessageFieldType? = null

    fun readInt(): Int {
        check(cur == INT) { "Expected int field, but $cur found" }
        return data.readInt().also { done() }
    }

    fun readString(): String {
        check(cur == STR) { "Expected string field, but $cur found" }
        return data.readUTF().also { done() }
    }

    fun readEnumName(): String {
        check(cur == ENUM) { "Expected enum field, but $cur found" }
        return data.readUTF().also { done() }
    }

    inline fun <reified T : Enum<T>> readEnum(): T = enumValueOf(readEnumName())

    val cur: MessageFieldType
        get() = curField ?: run {
            val b = data.read()
            when (b) {
                -1 -> END
                in 1 until FIELD_TYPES.size -> FIELD_TYPES[b]
                else -> error("Unexpected field type $b")
            }
        }.also {
            curField = it
        }

    private fun done() {
        curField = null
    }
}

/**
 * Type of the message field.
 */
enum class MessageFieldType { END, INT, STR, ENUM }

private val FIELD_TYPES = enumValues<MessageFieldType>()

/**
 * Builds the message.
 */
@Suppress("FunctionName")
inline fun Message(builder: MessageBuilder.() -> Unit): Message =
    MessageBuilder().apply { builder() }.build()

/**
 * Parses the message.
 */
inline fun <T> Message.parse(parser: MessageParser.() -> T): T =
    MessageParser(this).run { parser() }

