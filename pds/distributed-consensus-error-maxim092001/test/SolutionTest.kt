package consensus

import org.junit.*

class SolutionTest {
    @Test
    fun testSolution() {
        check(SOLUTION_FILE.exists()) { "Expecting solution in file: $SOLUTION_FILE"}
        val model = Model(4)
        model.load(SOLUTION_FILE)
        // check that solution has conflicting consensus
        val consensusSet = model.actions
            .filterIsInstance<Consensus>()
            .map { it.consensus }
            .toSet()
        check(consensusSet.size > 1) { "Consensus violation was not found" }
        println("Consensus violation: $consensusSet")
    }
}