package dijkstra.system.runtime

import dijkstra.messages.Message

interface Runtime {
    data class MessageInProgress(val dstId: Int, val srcId: Int, val message: Message)

    fun getMessageToPass(): MessageInProgress?

    fun sendMessage(srcId: Int, dstId: Int, message: Message)

    fun finishExecution()

    fun isFinished(): Boolean
}