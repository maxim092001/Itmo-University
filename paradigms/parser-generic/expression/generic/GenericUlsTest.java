package expression.generic;

/**
 * Generic unchecked int, long, short test.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class GenericUlsTest extends GenericTest {
    public GenericUlsTest() {
        uls(
                "10",
                (x, y, z) -> 10,
                (x, y, z) -> 10L,
                (x, y, z) -> s(10)
        );
        uls(
                "x",
                (x, y, z) -> x,
                (x, y, z) -> (long) x,
                (x, y, z) -> s(x)
        );
        uls(
                "y + 2",
                (x, y, z) -> y + 2,
                (x, y, z) -> y + 2L,
                (x, y, z) -> s(y + 2)
        );
        uls(
                "z / 2",
                (x, y, z) -> z / 2,
                (x, y, z) -> z / 2L,
                (x, y, z) -> s(z / 2)
        );
        uls(
                "y / z",
                (x, y, z) -> y / z,
                (x, y, z) -> y / (long) z,
                (x, y, z) -> s(y / z)
        );
        uls(
                "100 * x * y * 100 + z",
                (x, y, z) -> i(100 * x * y * 100 + z),
                (x, y, z) -> 100L * x * y * 100 + z,
                (x, y, z) -> s(100 * x * y * 100 + z)
        );
        uls(
                "x * y + (z - 1) / 10",
                (x, y, z) -> x * y + (z - 1) / 10,
                (x, y, z) -> x * (long) y + (z - 1L) / 10,
                (x, y, z) -> s(x * y + (z - 1) / 10)
        );
    }

    protected static short s(final int x) {
        return (short) x;
    }

    protected void uls(final String expression, final F<Integer> fu, final F<Long> fl, final F<Short> fs) {
        test(expression, "u", fu);
        test(expression, "l", fl);
        test(expression, "s", (x, y, z) -> fs.apply((short) x, (short) y, (short) z));
    }

    public static void main(final String[] args) {
        new GenericUlsTest().run();
    }
}
