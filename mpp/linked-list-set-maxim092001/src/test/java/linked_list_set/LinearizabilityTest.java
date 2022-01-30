package linked_list_set;

import org.jetbrains.kotlinx.lincheck.LinChecker;
import org.jetbrains.kotlinx.lincheck.annotations.Operation;
import org.jetbrains.kotlinx.lincheck.annotations.Param;
import org.jetbrains.kotlinx.lincheck.paramgen.IntGen;
import org.jetbrains.kotlinx.lincheck.strategy.stress.StressCTest;
import org.junit.Test;

@StressCTest(sequentialSpecification = SequentialSetImpl.class)
@Param(name = "key", gen = IntGen.class, conf = "1:5")
public class LinearizabilityTest {
    private Set set = new SetImpl();

    @Operation(params = "key")
    public boolean add(int x) {
        return set.add(x);
    }

    @Operation(params = "key")
    public boolean contains(int x) {
        return set.contains(x);
    }

    @Operation(params = "key")
    public boolean remove(int x) {
        return set.remove(x);
    }

    @Test
    public void test() {
        LinChecker.check(LinearizabilityTest.class);
    }
}