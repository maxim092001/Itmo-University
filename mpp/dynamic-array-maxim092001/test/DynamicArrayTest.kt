import org.jetbrains.kotlinx.lincheck.LoggingLevel.INFO
import org.jetbrains.kotlinx.lincheck.annotations.Operation
import org.jetbrains.kotlinx.lincheck.annotations.Param
import org.jetbrains.kotlinx.lincheck.check
import org.jetbrains.kotlinx.lincheck.paramgen.IntGen
import org.jetbrains.kotlinx.lincheck.strategy.stress.StressOptions
import org.jetbrains.kotlinx.lincheck.verifier.VerifierState
import org.junit.Test

@Param(name = "index", gen = IntGen::class, conf = "0:6")
class DynamicArrayTest {
    private val q = DynamicArrayImpl<Int>()

    @Operation(handleExceptionsAsResult = [IllegalArgumentException::class])
    fun get(@Param(name = "index") index: Int) = q.get(index)

    @Operation(handleExceptionsAsResult = [IllegalArgumentException::class])
    fun put(@Param(name = "index") index: Int, element: Int) = q.put(index, element)

    @Operation(handleExceptionsAsResult = [IllegalArgumentException::class])
    fun pushBack(@Param(name = "index") element: Int) = q.pushBack(element)

    @Operation
    fun size() = q.size

    @Test
    fun runTest() = StressOptions()
        .iterations(100)
        .invocationsPerIteration(50_000)
        .actorsBefore(2)
        .actorsAfter(2)
        .threads(3)
        .actorsPerThread(3)
        .sequentialSpecification(DynamicArrayIntSequential::class.java)
        .logLevel(INFO)
        .check(this::class.java)
}

class DynamicArrayIntSequential : VerifierState() {
    private val array = ArrayList<Int>()

    fun get(index: Int): Int =
        if (index < array.size) array[index]
        else throw IllegalArgumentException()

    fun put(index: Int, element: Int): Unit =
        if (index < array.size) array[index] = element
        else throw IllegalArgumentException()

    fun pushBack(element: Int) {
        array.add(element)
    }

    fun size(): Int = array.size

    override fun extractState() = array
}