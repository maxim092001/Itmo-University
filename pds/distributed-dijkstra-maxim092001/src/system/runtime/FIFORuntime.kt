package dijkstra.system.runtime

import dijkstra.messages.Message
import java.util.*
import kotlin.random.Random

class FIFORuntime : AbstractRuntime() {
    private val messages = mutableMapOf<Pair<Int, Int>, Queue<Message>>()

    override fun getMessageToPass(): Runtime.MessageInProgress? {
        if (messages.isEmpty()) {
            return null
        }
        val allPairs = messages.keys.toList()
        val idx = Random(System.currentTimeMillis()).nextInt(from = 0, until = allPairs.size)
        val pair = allPairs[idx]
        val queue = messages[pair] ?: throw AssertionError()
        assert(queue.isNotEmpty())
        val msg = queue.remove()
        val (srcId, dstId) = pair
        if (queue.isEmpty()) {
            messages.remove(pair)
        }
        return Runtime.MessageInProgress(srcId = srcId, dstId = dstId, message = msg)
    }

    override fun sendMessage(srcId: Int, dstId: Int, message: Message) {
        val curPair = Pair(srcId, dstId)
        val curQueue = messages[curPair]
        if (curQueue != null) {
            curQueue.add(message)
        } else {
            val newQueue = ArrayDeque<Message>()
            newQueue.add(message)
            messages[curPair] = newQueue
        }
    }
}