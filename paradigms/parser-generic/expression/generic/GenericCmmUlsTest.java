package expression.generic;

/**
 * Count, min, max over unchecked int, long, short test.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class GenericCmmUlsTest extends GenericUlsTest {
    public GenericCmmUlsTest() {
        GenericCmmTest.addCmm(this);
        ulsConst("2 min 3", 2);
        ulsConst("4 min 2 min 3", 2);
        ulsConst("2 max 3", 3);
        ulsConst("4 max 2 max 3", 4);
        ulsConst("20 min 3 + 3", 6);
        ulsConst("3 * 3 min 20", 9);
        ulsConst("4 min 2 max 3", 3);
        ulsConst("1 max 2 min 3", 2);
        ulsConst("6 - 10 min-4", -4);
        ulsConst("6 - 10 min-5", -5);
        uls(
                "x min y * z",
                (x, y, z) -> Math.min(x, y * z),
                (x, y, z) -> Math.min(x, y * (long) z),
                (x, y, z) -> (short) Math.min(x, (short) (y * z))
        );
        uls(
                "2 min x + 1",
                (x, y, z) -> Math.min(2, x + 1),
                (x, y, z) -> Math.min(2, x + 1L),
                (x, y, z) -> (short) (Math.min(2, x + 1))
        );
        uls(
                "x min y min z",
                (x, y, z) -> Math.min(Math.min(x, y), z),
                (x, y, z) -> (long) Math.min(Math.min(x, y), z),
                (x, y, z) -> (short) Math.min(Math.min(x, y), z)
        );
        uls(
                "x max y max z",
                (x, y, z) -> Math.max(Math.max(x, y), z),
                (x, y, z) -> (long) Math.max(Math.max(x, y), z),
                (x, y, z) -> (short) Math.max(Math.max(x, y), z)
        );
        uls(
                "x min y max z",
                (x, y, z) -> Math.max(Math.min(x, y), z),
                (x, y, z) -> (long) Math.max(Math.min(x, y), z),
                (x, y, z) -> (short) Math.max(Math.min(x, y), z)
        );
        uls(
                "x max y min z",
                (x, y, z) -> Math.min(Math.max(x, y), z),
                (x, y, z) -> (long) Math.min(Math.max(x, y), z),
                (x, y, z) -> (short) Math.min(Math.max(x, y), z)
        );

        uls(
                "count 5",
                (x, y, z) -> 2,
                (x, y, z) -> lCount(5),
                (x, y, z) -> sCount(5)
        );
        uls(
                "count -5",
                (x, y, z) -> uCount(-5),
                (x, y, z) -> lCount(-5),
                (x, y, z) -> sCount(-5)
        );
        uls("count (x - y)",
                (x, y, z) -> uCount(x - y),
                (x, y, z) -> lCount(x - (long) y),
                (x, y, z) -> sCount(x - y)
        );
        uls(
                "x -count y",
                (x, y, z) -> x - Integer.bitCount(y),
                (x, y, z) -> x - lCount(y),
                (x, y, z) -> (short) (x - sCount(y))
        );
        uls(
                "count -y",
                (x, y, z) -> uCount(-y),
                (x, y, z) -> lCount(-(long) y),
                (x, y, z) -> sCount(-y)
        );
        uls(
                "count(y * z)",
                (x, y, z) -> uCount(y * z),
                (x, y, z) -> lCount(y * (long) z),
                (x, y, z) -> sCount(y * z)
        );
    }

    private void ulsConst(final String expression, final int v) {
        uls(expression, (x, y, z) -> v, (x, y, z) -> (long) v, (x, y, z) -> (short) v);
    }

    private static Short sCount(final int v) {
        return (short) Integer.bitCount(v & 0xffff);
    }

    private static Integer uCount(final int v) {
        return Integer.bitCount(v);
    }

    private static Long lCount(final long v) {
        return (long) Long.bitCount(v);
    }

    public static void main(final String[] args) {
        new GenericCmmUlsTest().run();
    }
}
