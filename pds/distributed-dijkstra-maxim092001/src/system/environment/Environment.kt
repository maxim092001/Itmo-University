package dijkstra.system.environment

import dijkstra.messages.Message

interface Environment {
    val pid: Int

    fun send(dstId: Int, message: Message)

    val neighbours: Map<Int, Long>

    fun finishExecution()
}