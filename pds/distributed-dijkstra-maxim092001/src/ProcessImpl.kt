package dijkstra

import dijkstra.messages.*
import dijkstra.system.environment.Environment

class ProcessImpl(private val environment: Environment) : Process {

    private var initiated: Boolean = false
    private var distance: Long? = null
    private var acknowledgementBalance: Long = 0
    private var children: Long = 0
    private var parentId: Int? = null

    override fun onMessage(srcId: Int, message: Message) {
        when (message) {
            is AcknowledgementMessage -> acknowledgementBalance--
            is RemoveChildMessage -> children--
            is NewChildMessage -> children++
            is DistanceMessage -> {
                val messageDistance: Long = message.distance.value
                distance = when {
                    (distance != null && distance!! >= messageDistance) || distance == null -> messageDistance
                    else -> distance
                }
                if (distance == messageDistance) {
                    if (parentId != null) environment.send(parentId!!, RemoveChildMessage)
                    parentId = srcId
                    environment.send(srcId, NewChildMessage)
                    updateNeighbours()
                }
                environment.send(srcId, AcknowledgementMessage)
            }
        }
        finish()
    }

    override fun getDistance(): Long? = distance

    override fun startComputation() {
        distance = 0
        initiated = true
        updateNeighbours()
        finish()
    }

    private fun updateNeighbours(): Unit =
        environment.neighbours
            .filterNot { it.key == environment.pid }
            .forEach {
                environment.send(it.key, DistanceMessage(Values.Distance(it.value + distance!!)))
                acknowledgementBalance++
            }

    private fun finish() {
        if (acknowledgementBalance == 0L && children == 0L) {
            if (initiated) {
                environment.finishExecution()
            } else if (parentId != null) {
                environment.send(parentId!!, RemoveChildMessage)
                parentId = null
            }
        }
    }

}