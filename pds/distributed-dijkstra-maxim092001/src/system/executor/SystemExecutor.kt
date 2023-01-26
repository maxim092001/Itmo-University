package dijkstra.system.executor

import dijkstra.Process
import dijkstra.graph.Graph
import dijkstra.system.classloader.AntiCheatClassLoader
import dijkstra.system.environment.Environment
import dijkstra.system.environment.EnvironmentImpl
import dijkstra.system.runtime.Runtime

class SystemExecutor(private val graph: Graph, private val runtimeMaker: () -> Runtime) {
    private fun getProc(environment: Environment): Process {
        val procClass = AntiCheatClassLoader(Process::class.java.classLoader).loadClass("dijkstra.ProcessImpl")
        val constructors = procClass.declaredConstructors.filter {
            val argTypes = it.parameterTypes
            argTypes.size == 1 && argTypes[0] == Environment::class.java
        }
        assert(constructors.size == 1)
        return constructors[0].newInstance(environment) as Process
    }

    fun execute(sId: Int): List<Long?> {
        val runtime = runtimeMaker()
        val nProc = graph.graph.size
        val processes = (0 until nProc).map {
            val curEnv = EnvironmentImpl(graph = graph, pid = it, runtime = runtime)
            getProc(environment = curEnv)
        }

        processes[sId].startComputation()
        while (true) {
            val curMsg = runtime.getMessageToPass()
            if (runtime.isFinished() || curMsg == null) {
                break
            }
            processes[curMsg.dstId].onMessage(srcId = curMsg.srcId, message = curMsg.message)
        }

        assert(runtime.isFinished())
        return processes.map { it.getDistance() }
    }
}