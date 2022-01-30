package msqueue;

import org.jetbrains.kotlinx.lincheck.LinChecker;
import org.jetbrains.kotlinx.lincheck.annotations.Operation;
import org.jetbrains.kotlinx.lincheck.strategy.stress.StressCTest;
import org.junit.Test;

@StressCTest(sequentialSpecification = SequentialQueue.class)
public class LinearizabilityTest {
    private final Queue queue = new MSQueue();

    @Operation
    public void enqueue(int x) {
        queue.enqueue(x);
    }

    @Operation
    public int peek() {
        return queue.peek();
    }

    @Operation
    public int dequeue() {
        return queue.dequeue();
    }

    @Test
    public void test() {
        LinChecker.check(LinearizabilityTest.class);
    }
}
