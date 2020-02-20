package test;

import java.util.List;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class ExceptionsPowLogTest extends ExceptionsTest {
    private static final Reason POWER = new Reason("power");
    private static final Reason LOG = new Reason("log");

    protected ExceptionsPowLogTest() {
        levels.add(list(
                op("**", ExceptionsPowLogTest::power),
                op("//", ExceptionsPowLogTest::log)
        ));

        tests.addAll(List.of(
                op("2 ** 3", (x, y, z) -> 8),
                op("4 ** 3 ** 2", (x, y, z) -> 4096),
                op("2 ** 3 * 3", (x, y, z) -> 24),
                op("x ** (y * z)", (x, y, z) -> power(x, y * z)),
                op("2 ** x + 1", (x, y, z) -> power(2, x) + 1),
                op("-1 ** (3 ** x)", (x, y, z) -> x < 0 ? error(POWER) : -1),
                op("8 // 2", (x, y, z) -> 3),
                op("x // y", (x, y, z) -> log(x, y))
        ));
    }

    private static long log(final long a, final long b) {
        try {
            final double result = Math.log(a) / Math.log(b);
            return a > 0 && b > 0 && result == result ? (long) result : error(LOG);
        } catch (final ArithmeticException e) {
            return error(LOG);
        }
    }

    private static long power(final long a, long b) {
        if (b < 0 || a == 0 && b == 0) {
            return error(POWER);
        }
        if (Math.abs(a) <= 1 && b > 2) {
            b = (b - 1) % 2 + 1;
        }

        double result = 1;
        for (int i = 0; i < b; i++) {
            result *= a;
            if (result < Integer.MIN_VALUE || Integer.MAX_VALUE < result) {
                return error(OVERFLOW);
            }
        }
        return (long) result;
    }

    public static void main(final String[] args) {
        new ExceptionsPowLogTest().run();
    }
}
