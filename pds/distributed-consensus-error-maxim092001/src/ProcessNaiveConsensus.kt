package consensus

import java.util.*
import kotlin.collections.ArrayDeque

class ProcessNaiveConsensus(env: Environment) : AbstractProcess(env) {
    private val nFails = 1
    private val nPhases = nFails + 1

    // start with our processId as a proposal
    private val proposals = TreeSet<Int>().apply { add(env.processId) }

    // messages for future phases are queued here
    private val queue = HashMap<Int, ArrayDeque<Message>>()

    override suspend fun run() {
        // Repeat the protocol for nPhases
        for (phase in 1..nPhases) {
            // Broadcast proposals
            for (i in 1..env.nProcesses) {
                if (i != env.processId) {
                    env.send(i) {
                        writeInt(phase) // phase number goes first
                        writeInt(proposals.size)
                        proposals.forEach { writeInt(it) }
                    }
                }
            }
            // Wait for nProcesses - nFails - 1 answers from this phase
            var remaining = env.nProcesses - nFails - 1
            while (remaining > 0) {
                // take a message for this phase from the queue
                val message = queue[phase]?.removeFirstOrNull() ?:
                    nextMessage().message // or wait until a message is received
                // parse message
                message.parse {
                    val messagePhase = readInt()
                    if (messagePhase < phase) { // old phase -- ignore message
                        return@parse
                    }
                    if (messagePhase > phase) { // next phase -- queue
                        queue.getOrPut(messagePhase) { ArrayDeque() }.add(message)
                        return@parse
                    }
                    // process the message if it is for this phase
                    val nProposals = readInt()
                    repeat(nProposals) { proposals += readInt() }
                    remaining--
                }
            }
        }
        // Done all phases -- decide on the minimal proposed value
        env.decide(proposals.minOrNull()!!)
    }

    override fun toString(): String = proposals.toString()
}