import mutex.*
import org.junit.*

class SolutionTest {
    @Test
    fun testSolution() {
        check(SOLUTION_FILE.exists()) { "Expecting solution in file: $SOLUTION_FILE"}
        val model = Model(2)
        model.load(SOLUTION_FILE)
        // check that solution has overlapping locks
        val overlap = model.actions
            .filterIsInstance<Unlock>()
            .map { Range(it.from.time, it.time) }
            .sortedBy { it.from }
            .zipWithNext()
            .firstOrNull { it.first.to > it.second.from }
        check(overlap != null) { "Overlapping pair of locks was not found" }
        println("Locks overlap: $overlap")
    }

    private data class Range(val from: Int, val to: Int)
}