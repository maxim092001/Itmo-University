package expression.generic;

import expression.BaseTest;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class GenericTest extends BaseTest {
    private static final int SIZE = 20;
    public static final int MAX = Integer.MAX_VALUE - 1;
    public static final int MIN = Integer.MIN_VALUE;

    protected final List<Op<F<?>>> tests = new ArrayList<>();
    private final Tabulator tabulator = new GenericTabulator();

    public GenericTest() {
        allConst("10", 10);
        allConst("10 + 4 / 2 - 7", 5);
        all(
                "x",
                (x, y, z) -> (long) x,
                (x, y, z) -> (double) x,
                (x, y, z) -> bi(x)
        );
        all(
                "y + 2",
                (x, y, z) -> y + 2L,
                (x, y, z) -> y + 2.0,
                (x, y, z) -> bi(y).add(bi(2))
        );
        all(
                "z / 2",
                (x, y, z) -> z / 2L,
                (x, y, z) -> z / 2.0,
                (x, y, z) -> bi(z / 2)
        );
        all(
                "y / z",
                (x, y, z) -> y / (long) z,
                (x, y, z) -> y / (double) z,
                (x, y, z) -> bi(y / z)
        );
        all(
                "10000000 * x * y * 10000000 + z",
                (x, y, z) -> mul(mul(mul(10000000, x), y), 10000000) + z,
                (x, y, z) -> 10000000.0 * x * y * 10000000.0 + z,
                (x, y, z) -> bi(10000000).multiply(bi(x)).multiply(bi(y)).multiply(bi(10000000)).add(bi(z))
        );
        all(
                "x * y + (z - 1) / 10",
                (x, y, z) -> mul(x, y) + check(z - 1) / 10L,
                (x, y, z) -> x * (double) y + (z - 1.0) / 10,
                (x, y, z) -> bi(x).multiply(bi(y)).add(bi(z).subtract(BigInteger.ONE).divide(BigInteger.TEN))
        );
    }

    protected static long mul(final long a, final long b) {
        return check(a * b);
    }

    protected static long check(final long v) {
        if (v != (int) v) {
            throw new RuntimeException("Overflow");
        }
        return v;
    }

    protected static BigInteger bi(final int v) {
        return BigInteger.valueOf(v);
    }

    protected static Integer i(final long x) {
        return Integer.MIN_VALUE <= x && x <= Integer.MAX_VALUE ? (int) x : null;
    }

    protected void all(final String expression, final F<Long> fi, final F<Double> fd, final F<BigInteger> fbi) {
        test(expression, "i", (x, y, z) -> (int) check(fi.apply(x, y, z)));
        test(expression, "d", fd);
        test(expression, "bi", fbi);
    }

    protected void allConst(final String expression, final Integer v) {
        final BigInteger bi = bi(v);
        all(expression, (x, y, z) -> (long) v, (x, y, z) -> (double) v, (x, y, z) -> bi);
    }

    protected void test(final String expression, final String name, final F<?> f) {
        tests.add(op(name + ": " + expression, f));
    }

    @Override
    public void test() {
        for (final Op<F<?>> test : tests) {
            final String[] parts = test.name.split(": ");
            test(
                    parts[0], parts[1], test.f,
                    -randomInt(SIZE), randomInt(SIZE),
                    -randomInt(SIZE), randomInt(SIZE),
                    -randomInt(SIZE), randomInt(SIZE)
            );
            test(
                    parts[0], parts[1], test.f,
                    MAX - randomInt(SIZE), MAX,
                    MAX - randomInt(SIZE), MAX,
                    MAX - randomInt(SIZE), MAX
            );
            test(
                    parts[0], parts[1], test.f,
                    MIN, MIN + randomInt(SIZE),
                    MIN, MIN + randomInt(SIZE),
                    MIN, MIN + randomInt(SIZE)
            );
        }
    }

    private void test(final String mode, final String expression, final F<?> f, final int x1, final int x2, final int y1, final int y2, final int z1, final int z2) {
        System.out.format("mode=%s, x=[%d, %d] y=[%d, %d] z=[%d, %d], expression=%s%n", mode, x1, x2, y1, y2, z1, z2, expression);
        final Object[][][] result;
        try {
            result = tabulator.tabulate(mode, expression, x1, x2, y1, y2, z1, z2);
        } catch (final Exception e) {
            throw new AssertionError(e);
        }
        for (int x = x1; x <= x2; x++) {
            for (int y = y1; y <= y2; y++) {
                for (int z = z1; z <= z2; z++) {
                    counter.nextTest();
                    Object expected;
                    try {
                        expected = f.apply(x, y, z);
                    } catch (final RuntimeException e) {
                        expected = null;
                    }
                    final Object actual = result[x - x1][y - y1][z - z1];
                    assert Objects.equals(expected, actual) :
                            String.format("table[%d][%d][%d](x=%d, y=%d, z=%d]) = %s (expected %s)",
                                    x - x1, y - y1, z - z1,
                                    x, y, z,
                                    actual, expected
                            );
                    counter.passed();
                }
            }
        }
    }

    public static void main(final String[] args) {
        new GenericTest().run();
    }

    protected interface F<T> {
        T apply(int x, int y, int z);
    }
}
