import org.jetbrains.kotlinx.lincheck.*
import org.jetbrains.kotlinx.lincheck.LoggingLevel.*
import org.jetbrains.kotlinx.lincheck.annotations.Operation
import org.jetbrains.kotlinx.lincheck.strategy.managed.modelchecking.*
import org.jetbrains.kotlinx.lincheck.strategy.stress.*
import org.jetbrains.kotlinx.lincheck.verifier.*
import org.junit.*
import java.util.*
import kotlin.collections.ArrayList

class FCPriorityQueueTest {
    private val q = FCPriorityQueue<Int>()

    @Operation
    fun poll(): Int? = q.poll()

    @Operation
    fun peek(): Int? = q.peek()

    @Operation
    fun add(element: Int): Unit = q.add(element)

    @Test
    fun modelCheckingTest() = ModelCheckingOptions()
        .iterations(100)
        .invocationsPerIteration(10_000)
        .threads(3)
        .actorsPerThread(3)
        .sequentialSpecification(PriorityQueueSequential::class.java)
        .logLevel(INFO)
        .check(this::class.java)

    @Test
    fun stressTest() = StressOptions()
        .iterations(100)
        .invocationsPerIteration(50_000)
        .actorsBefore(2)
        .actorsAfter(2)
        .threads(3)
        .actorsPerThread(3)
        .sequentialSpecification(PriorityQueueSequential::class.java)
        .logLevel(INFO)
        .check(this::class.java)
}

class PriorityQueueSequential : VerifierState() {
    private val q = PriorityQueue<Int>()

    fun poll(): Int? = q.poll()
    fun peek(): Int? = q.peek()
    fun add(element: Int) { q.add(element) }

    override fun extractState() = ArrayList(q)
}