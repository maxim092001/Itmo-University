package sum;

import java.util.Arrays;
import java.util.Random;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class SumDoubleTest extends SumTest {
    public SumDoubleTest(final SChecker checker) {
        super(checker);
    }

    public static void main(final String... args) {
        new SumDoubleTest(new SumDoubleChecker("SumDouble")).run();
    }

    @Override
    protected void test() {
        test(1, "1");
        test(6, "1", "2", "3");
        test(1, " 1");
        test(1, "1 ");
        test(1, "\u20001\u2000");
        test(12345, "\u200012345\u2000");
        test(1368, " 123 456 789 ");
        test(60, "010", "020", "030");
        test(-1, "-1");
        test(-6, "-1", "-2", "-3");
        test(-12345, "\u2000-12345\u2000");
        test(-1368, " -123 -456 -789 ");
        test(1, "+1");
        test(6, "+1", "+2", "+3");
        test(12345, "\u2000+12345\u2000");
        test(1368, " +123 +456 +789 ");
        test(0);
        test(0, " ");
        test(5, "2.5 2.5");
        test(0, "1e100 -1e100");
        test(2e100, "1.5e100 0.5e100");
        randomTest(10, 100);
        randomTest(10, 0.01);
        randomTest(10, Long.MIN_VALUE);
        randomTest(10, Long.MAX_VALUE);
        randomTest(10, Double.MAX_VALUE / 10);
        randomTest(100, Double.MAX_VALUE / 100);
    }

    @Override
    protected Number randomValue(final Number max, final Random random) {
        return (checker.getRandom().nextDouble() - 0.5) * 2 * max.doubleValue();
    }

    @Override
    protected Number sum(final Number[] values) {
        return Arrays.stream(values).mapToDouble(Number::doubleValue).sum();
    }
}
