package raft.system

import kotlinx.serialization.*
import org.slf4j.*
import raft.*
import java.io.*

private const val C_STATE = 'S'
private const val C_LOG = 'L'

class PersistentStorage(processId: Int) : Storage() {
    private val logger: Logger = LoggerFactory.getLogger("Storage")
    private val file = File("node$processId.storage.txt")

    init {
        parse()
    }

    private fun parse() {
        try {
            file.forEachLine { line ->
                check(line.getOrNull(1) == ':') { "Malformed line '$line' in $file" }
                when (line[0]) {
                    C_STATE -> super.writePersistentState(json.decodeFromString(line.substring(2)))
                    C_LOG -> super.appendLogEntry(json.decodeFromString(line.substring(2)))
                    else -> error("Unknown line '$line' in $file")
                }
            }
        } catch (e: FileNotFoundException) {
            /* do nothing */
        }
    }

    private fun append(str: String) =
        file.appendText("$str\n")

    override fun writePersistentState(state: PersistentState) {
        super.writePersistentState(state)
        append("$C_STATE:${json.encodeToString(state)}")
    }

    override fun appendLogEntry(entry: LogEntry) {
        super.appendLogEntry(entry)
        val str = json.encodeToString(entry)
        logger.info("appendLogEntry $str")
        append("$C_LOG:$str")
    }
}