import org.jetbrains.kotlinx.lincheck.annotations.Operation
import org.jetbrains.kotlinx.lincheck.check
import org.jetbrains.kotlinx.lincheck.strategy.managed.modelchecking.ModelCheckingOptions
import org.jetbrains.kotlinx.lincheck.strategy.stress.StressOptions
import org.jetbrains.kotlinx.lincheck.verifier.VerifierState
import org.junit.Test
import kotlin.coroutines.Continuation
import kotlin.coroutines.resume
import kotlin.coroutines.suspendCoroutine

class SynchronousQueueTest : SynchronousQueue<Int> {
    private val q = SynchronousQueueMS<Int>()

    @Operation(cancellableOnSuspension = false)
    override suspend fun send(element: Int) {
        q.send(element)
    }

    @Operation(cancellableOnSuspension = false)
    override suspend fun receive(): Int = q.receive()

    @Test
    fun stressTest() = StressOptions()
        .iterations(100)
        .invocationsPerIteration(30_000)
        .actorsBefore(0)
        .actorsAfter(0)
        .threads(3)
        .actorsPerThread(3)
        .sequentialSpecification(SynchronousQueueSequential::class.java)
        .check(this::class.java)

    @Test
    fun modelCheckingTest() = ModelCheckingOptions()
        .iterations(100)
        .invocationsPerIteration(30_000)
        .actorsBefore(0)
        .actorsAfter(0)
        .threads(3)
        .actorsPerThread(3)
        .sequentialSpecification(SynchronousQueueSequential::class.java)
        .check(this::class.java)
}

class SynchronousQueueSequential : SynchronousQueue<Int>, VerifierState() {
    private val senders = ArrayList<Pair<Continuation<Unit>, Int>>() // pair = continuation + element
    private val receivers = ArrayList<Continuation<Int>>()

    override suspend fun send(element: Int) {
        if (receivers.isNotEmpty()) {
            val r = receivers.removeAt(0)
            r.resume(element)
        } else {
            suspendCoroutine<Unit> { cont ->
                senders.add(cont to element)
            }
        }
    }

    override suspend fun receive(): Int {
        if (senders.isNotEmpty()) {
            val (s, elem) = senders.removeAt(0)
            s.resume(Unit)
            return elem
        } else {
            return suspendCoroutine { cont ->
                receivers.add(cont)
            }
        }
    }

    override fun extractState() = Unit
}