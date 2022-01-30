import org.jetbrains.kotlinx.lincheck.*
import org.jetbrains.kotlinx.lincheck.annotations.*
import org.jetbrains.kotlinx.lincheck.annotations.Operation
import org.jetbrains.kotlinx.lincheck.execution.*
import org.jetbrains.kotlinx.lincheck.paramgen.*
import org.jetbrains.kotlinx.lincheck.strategy.managed.modelchecking.*
import org.jetbrains.kotlinx.lincheck.strategy.stress.*
import org.jetbrains.kotlinx.lincheck.verifier.*
import org.junit.*
import kotlin.reflect.jvm.*

@Param.Params(
    Param(name = "index", gen = IntGen::class, conf = "0:4"),
    Param(name = "value", gen = IntGen::class, conf = "0:2")
)
class AtomicArrayTest {
    private val a = AtomicArray(5, 0)

    @Operation(params = ["index"])
    fun get(index: Int) =
        a.get(index)

    @Operation(params = ["index", "value", "value"])
    fun cas(index: Int, expected: Int, update: Int) =
        a.cas(index, expected, update)

    @Operation(params = ["index", "value", "value",
                         "index", "value", "value"])
    fun cas2(index1: Int, expected1: Int, update1: Int,
             index2: Int, expected2: Int, update2: Int) =
        a.cas2(index1, expected1, update1, index2, expected2, update2)

    @Test
    fun stressTest() = StressOptions()
        .iterations(100)
        .invocationsPerIteration(10_000)
        .actorsBefore(0)
        .actorsAfter(0)
        .threads(3)
        .actorsPerThread(3)
        .sequentialSpecification(AtomicArrayIntSequential::class.java)
        .check(this::class.java)

    @Test
    fun modelCheckingTest() = ModelCheckingOptions()
        .iterations(100)
        .invocationsPerIteration(10_000)
        .actorsBefore(0)
        .actorsAfter(0)
        .threads(2)
        .actorsPerThread(3)
        .sequentialSpecification(AtomicArrayIntSequential::class.java)
        .checkObstructionFreedom(true)
        .check(this::class.java)

    @Test
    fun modelCheckingTestCustomABA() = ModelCheckingOptions()
        .executionGenerator(CustomABAScenarioGenerator::class.java)
        .iterations(    1)
        .invocationsPerIteration(100_000)
        .sequentialSpecification(AtomicArrayIntSequential::class.java)
        .checkObstructionFreedom(true)
        .minimizeFailedScenario(false)
        .check(this::class.java)
}

class CustomABAScenarioGenerator(testConfiguration: CTestConfiguration, testStructure: CTestStructure)
    : ExecutionGenerator(testConfiguration, testStructure)
{
    override fun nextExecution() = ExecutionScenario(
        listOf(),
        listOf(
            listOf(
                Actor(AtomicArrayTest::cas2.javaMethod!!, listOf(0, 0, 1, 1, 0, 1))
            ),
            listOf(
                Actor(AtomicArrayTest::get.javaMethod!!, listOf(0)), // it should help the cas2 above
                Actor(AtomicArrayTest::cas.javaMethod!!, listOf(1, 1, 0)), // we expect it to replace `1` with `0`
                Actor(AtomicArrayTest::get.javaMethod!!, listOf(1)), // we expect it to read `0`. however, due to the ABA
                                                                     // problem it can read `1` with an incorrect implementation
        )
        ),
        listOf()
    )

}

class AtomicArrayIntSequential : VerifierState() {
    private val a = arrayOfNulls<Int>(5)

    init {
        for (i in 0 until 5) a[i] = 0
    }

    fun get(index: Int): Int? = a[index]

    fun set(index: Int, value: Int) {
        a[index] = value
    }

    fun cas(index: Int, expected: Int, update: Int): Boolean {
        if (a[index] != expected) return false
        a[index] = update
        return true
    }

    fun cas2(index1: Int, expected1: Int, update1: Int,
             index2: Int, expected2: Int, update2: Int): Boolean {
        if (a[index1] != expected1 || a[index2] != expected2) return false
        a[index1] = update1
        a[index2] = update2
        return true
    }

    override fun extractState() = a.toList()
}
