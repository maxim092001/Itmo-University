package dijkstra.system.runtime

abstract class AbstractRuntime : Runtime {
    private var finished: Boolean = false

    override fun finishExecution() {
        if (finished) {
            throw IllegalStateException("Cannot finish finished execution")
        }
        finished = true
    }

    override fun isFinished(): Boolean = finished
}