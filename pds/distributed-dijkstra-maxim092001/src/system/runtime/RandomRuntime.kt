package dijkstra.system.runtime

import dijkstra.messages.Message

class RandomRuntime : AbstractRuntime() {
    private val messages = ArrayList<Runtime.MessageInProgress>()

    override fun getMessageToPass(): Runtime.MessageInProgress? {
        messages.shuffle()
        return messages.removeLastOrNull()
    }

    override fun sendMessage(srcId: Int, dstId: Int, message: Message) {
        val addRes = messages.add(Runtime.MessageInProgress(dstId = dstId, srcId = srcId, message = message))
        assert(addRes)
    }
}