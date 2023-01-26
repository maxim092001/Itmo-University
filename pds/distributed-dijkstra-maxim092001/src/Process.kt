package dijkstra

import dijkstra.messages.Message

interface Process {
    fun onMessage(srcId: Int, message: Message)

    fun getDistance(): Long?

    fun startComputation()
}