package stack;

import org.jetbrains.kotlinx.lincheck.LinChecker;
import org.jetbrains.kotlinx.lincheck.Options;
import org.jetbrains.kotlinx.lincheck.annotations.Operation;
import org.jetbrains.kotlinx.lincheck.strategy.stress.StressOptions;
import org.junit.Test;

public class LinearizabilityTest {
    private Stack stack = new StackImpl();

    @Operation
    public void push(int x) {
        stack.push(x);
    }

    @Operation
    public int pop() {
        return stack.pop();
    }

    @Test
    public void test() {
        Options options = new StressOptions().sequentialSpecification(SequentialStack.class);
        LinChecker.check(LinearizabilityTest.class, options);
    }
}
