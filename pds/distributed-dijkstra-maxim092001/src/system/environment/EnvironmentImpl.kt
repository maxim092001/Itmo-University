package dijkstra.system.environment

import dijkstra.graph.Graph
import dijkstra.messages.Message
import dijkstra.system.runtime.Runtime

class EnvironmentImpl(override val pid: Int, private val graph: Graph, private val runtime: Runtime) : Environment {
    init {
        assert(pid in graph.graph.indices)
    }

    override fun send(dstId: Int, message: Message) {
        assert(!runtime.isFinished())
        require(neighbours.containsKey(dstId) || graph.graph[dstId].containsKey(pid))
        runtime.sendMessage(srcId = pid, dstId = dstId, message = message)
    }

    override val neighbours: Map<Int, Long> = graph.graph[pid]

    override fun finishExecution() {
        runtime.finishExecution()
    }
}