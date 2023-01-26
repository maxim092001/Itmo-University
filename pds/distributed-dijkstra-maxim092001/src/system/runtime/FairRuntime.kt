package dijkstra.system.runtime

import dijkstra.messages.Message
import java.util.*

class FairRuntime : AbstractRuntime() {
    private val messages: Queue<Runtime.MessageInProgress> = ArrayDeque()

    override fun getMessageToPass(): Runtime.MessageInProgress? {
        return messages.poll()
    }

    override fun sendMessage(srcId: Int, dstId: Int, message: Message) {
        val insertResult = messages.offer(Runtime.MessageInProgress(srcId = srcId, dstId = dstId, message = message))
        assert(insertResult)
    }
}