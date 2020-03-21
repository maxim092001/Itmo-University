package expression.generic;

import java.math.BigInteger;

/**
 * Generic count, min, max tests.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class GenericCmmTest extends GenericTest {
    public GenericCmmTest() {
        addCmm(this);
    }

    static void addCmm(final GenericTest test) {
        test.allConst("2 min 3", 2);
        test.allConst("4 min 2 min 3", 2);
        test.allConst("2 max 3", 3);
        test.allConst("4 max 2 max 3", 4);
        test.allConst("20 min 3 + 3", 6);
        test.allConst("3 * 3 min 20", 9);
        test.allConst("4 min 2 max 3", 3);
        test.allConst("1 max 2 min 3", 2);
        test.allConst("6 - 10 min-4", -4);
        test.allConst("6 - 10 min-5", -5);
        test.all(
                "x min y * z",
                (x, y, z) -> Math.min(x, mul(y, z)),
                (x, y, z) -> Math.min(x, y * (double) z),
                (x, y, z) -> bi(x).min(bi(y).multiply(bi(z)))
        );
        test.all(
                "2 min x + 1",
                (x, y, z) -> Math.min(2, x + 1L),
                (x, y, z) -> Math.min(2, x + 1.0),
                (x, y, z) -> bi(Math.min(2, x + 1))
        );
        test.all(
                "x min y min z",
                (x, y, z) -> (long) Math.min(Math.min(x, y), z),
                (x, y, z) -> (double) Math.min(Math.min(x, y), z),
                (x, y, z) -> bi(Math.min(Math.min(x, y), z))
        );
        test.all(
                "x max y max z",
                (x, y, z) -> (long) Math.max(Math.max(x, y), z),
                (x, y, z) -> (double) Math.max(Math.max(x, y), z),
                (x, y, z) -> bi(Math.max(Math.max(x, y), z))
        );
        test.all(
                "x min y max z",
                (x, y, z) -> (long) Math.max(Math.min(x, y), z),
                (x, y, z) -> (double) Math.max(Math.min(x, y), z),
                (x, y, z) -> bi(Math.max(Math.min(x, y), z))
        );
        test.all(
                "x max y min z",
                (x, y, z) -> (long) Math.min(Math.max(x, y), z),
                (x, y, z) -> (double) Math.min(Math.max(x, y), z),
                (x, y, z) -> bi(Math.min(Math.max(x, y), z))
        );

        test.all(
                "count 5",
                (x, y, z) -> 2L,
                (x, y, z) -> dCount(5.0),
                (x, y, z) -> bi(2)
        );
        test.all(
                "count -5",
                (x, y, z) -> iCount(-5),
                (x, y, z) -> dCount(-5),
                (x, y, z) -> biCount(bi(-5))
        );
        test.all("count (x - y)",
                (x, y, z) -> iCount(check(x - y)),
                (x, y, z) -> dCount(x - y),
                (x, y, z) -> biCount(bi(x).subtract(bi(y)))
        );
        test.all(
                "x -count y",
                (x, y, z) -> x - (long) Integer.bitCount(y),
                (x, y, z) -> x - dCount(y),
                (x, y, z) -> bi(x).subtract(biCount(bi(y)))
        );
        test.all(
                "count -y",
                (x, y, z) -> iCount(check(-(long) y)),
                (x, y, z) -> dCount(-(double) y),
                (x, y, z) -> biCount(bi(y).negate())
        );
        test.all(
                "count(y * z)",
                (x, y, z) -> iCount((int) mul(y, z)),
                (x, y, z) -> dCount(y * (double) z),
                (x, y, z) -> biCount(bi(y).multiply(bi(z)))
        );
    }

    protected static Long iCount(final long v) {
        return (long) Integer.bitCount((int) check(v));
    }

    protected static BigInteger biCount(final BigInteger v) {
        return bi(v.bitCount());
    }

    protected static Double dCount(final double v) {
        return (double) Long.bitCount(Double.doubleToLongBits(v));
    }

    public static void main(final String[] args) {
        new GenericCmmTest().run();
    }
}
