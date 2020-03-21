package expression.generic;

/**
 * Generic unchecked int, long, short test.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class GenericLsTest extends GenericTest {
    public GenericLsTest() {
        ls(
                "10",
                (x, y, z) -> 10L,
                (x, y, z) -> s(10)
        );
        ls(
                "x",
                (x, y, z) -> (long) x,
                (x, y, z) -> s(x)
        );
        ls(
                "y + 2",
                (x, y, z) -> y + 2L,
                (x, y, z) -> s(y + 2)
        );
        ls(
                "z / 2",
                (x, y, z) -> z / 2L,
                (x, y, z) -> s(z / 2)
        );
        ls(
                "y / z",
                (x, y, z) -> y / (long) z,
                (x, y, z) -> s(y / z)
        );
        ls(
                "100 * x * y * 100 + z",
                (x, y, z) -> 100L * x * y * 100 + z,
                (x, y, z) -> s(100 * x * y * 100 + z)
        );
        ls(
                "x * y + (z - 1) / 10",
                (x, y, z) -> x * (long) y + (z - 1L) / 10,
                (x, y, z) -> s(x * y + (z - 1) / 10)
        );
    }

    protected static short s(final int x) {
        return (short) x;
    }

    protected void ls(final String expression, final F<Long> fl, final F<Short> fs) {
        test(expression, "l", fl);
        test(expression, "s", (x, y, z) -> fs.apply((short) x, (short) y, (short) z));
    }

    public static void main(final String[] args) {
        new GenericLsTest().run();
    }
}
