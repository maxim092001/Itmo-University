package consensus

import kotlin.coroutines.*

/**
 * Interface that process implementing mutual exclusion algorithm shall satisfy.
 * All functions are called from the single main thread.
 */
interface Process {
    /**
     * Called after construction so that process can send initial messages.
     */
    fun start()

    /**
     * Called on arriving [message] from another process [srcId] (from 1 to [Environment.nProcesses]).
     */
    fun onMessage(srcId: Int, message: Message)
}


/**
 * Abstract process implementation that supports writing sequential code for the process.
 */
@RestrictsSuspension
abstract class AbstractProcess(protected val env: Environment) : Process {
    var isRunning: Boolean = true
        private set
    private var nextMessageCont: Continuation<IncomingMessage>? = null

    /**
     * Contains the implementation of the process logic.
     */
    abstract suspend fun run()

    suspend fun nextMessage(): IncomingMessage = suspendCoroutine { cont ->
        nextMessageCont = cont
    }

    final override fun start() {
        ::run.startCoroutine(Continuation(EmptyCoroutineContext) {
            isRunning = false
        })
    }

    final override fun onMessage(srcId: Int, message: Message) {
        val cont = nextMessageCont ?: return
        nextMessageCont = null
        cont.resume(IncomingMessage(srcId, message))
    }
}

data class IncomingMessage(val srcId: Int, val message: Message)