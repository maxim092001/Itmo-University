package cljtest.linear;

import clojure.lang.IPersistentVector;
import jstest.Engine;

import java.util.Arrays;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class LinearNaryTest extends LinearBinaryTest {
    @Override
    protected void test() {
        super.test();

        final Engine.Result<IPersistentVector> v123 = vector(1, 2, 3);
        final Engine.Result<IPersistentVector> v12 = vector(1.1, 2.1);
        final Engine.Result<IPersistentVector> v34 = vector(3.1, 4.1);
        final Engine.Result<IPersistentVector> v56 = vector(5.1, 6.1);

        assertVector(V_ADD.call(v12), 1.1, 2.1);
        assertVector(V_SUB.call(v12), -1.1, -2.1);
        assertVector(V_MUL.call(v12), 1.1, 2.1);
        assertVector(V_BY_S.call(v12), 1.1, 2.1);
        assertVector(VECT.call(v123), 1, 2, 3);

        assertVector(V_ADD.call(v12, v34, v56), 9.3, 12.3);
        assertVector(V_SUB.call(v12, v34, v56), -7.1, -8.1);
        assertVector(V_MUL.call(v12, v34, v56), 17.391, 52.521);
        assertVector(V_BY_S.call(v12, number(10), number(2)), 22, 42);
        assertVector(VECT.call(v123, vector(4, 5, 6), vector(7, 8, 9)), 78, 6, -66);

        final Engine.Result<IPersistentVector> m123_456 = matrix(row(1.1, 2.1, 3.1), row(4.1, 5.1, 6.1));
        final Engine.Result<IPersistentVector> m789_012 = matrix(row(7.1, 8.1, 9.1), row(0.1, 1.1, 2.1));
        final Engine.Result<IPersistentVector> m345_678 = matrix(row(3.1, 4.1, 5.1), row(6.1, 7.1, 8.1));

        assertMatrix(M_ADD.call(m123_456), row(1.1, 2.1, 3.1), row(4.1, 5.1, 6.1));
        assertMatrix(M_SUB.call(m123_456), row(-1.1, -2.1, -3.1), row(-4.1, -5.1, -6.1));
        assertMatrix(M_MUL.call(m123_456), row(1.1, 2.1, 3.1), row(4.1, 5.1, 6.1));
        assertMatrix(M_BY_S.call(m123_456), row(1.1, 2.1, 3.1), row(4.1, 5.1, 6.1));
        assertMatrix(M_BY_M.call(m123_456), row(1.1, 2.1, 3.1), row(4.1, 5.1, 6.1));

        assertMatrix(M_ADD.call(m123_456, m789_012, m345_678), row(11.3, 14.3, 17.3), row(10.3, 13.3, 16.3));
        assertMatrix(M_SUB.call(m123_456, m789_012, m345_678), row(-9.1, -10.1, -11.1), row(-2.1, -3.1, -4.1));
        assertMatrix(M_MUL.call(m123_456, m789_012, m345_678), row(24.211, 69.741, 143.871), row(2.501, 39.831, 103.761));
        assertMatrix(M_BY_S.call(m123_456, number(5), number(2)), row(11, 21, 31), row(41, 51, 61));
        final Engine.Result<IPersistentVector> m789_012t = TRANSPOSE.call(m789_012);
        assertMatrix(M_BY_M.call(m123_456, m789_012t, m345_678), row(218.866, 280.826, 342.786), row(505.246, 650.006, 794.766));

        for (final Engine.Result<?> r : Arrays.asList(vector(1, 2, 3, 4), number(10), m789_012t)) {
            assertException(V_ADD.expectException(v12, r));
            assertException(V_SUB.expectException(v12, r));
            assertException(V_MUL.expectException(v12, r));

            assertException(SCALAR.expectException(v12, r));
            assertException(SCALAR.expectException(r, v12));

            assertException(VECT.expectException(v12, r));
            assertException(VECT.expectException(r, v12));
            assertException(VECT.expectException(v123, r));
            assertException(VECT.expectException(r, v123));

            assertException(M_ADD.expectException(m123_456, r));
            assertException(M_SUB.expectException(m123_456, r));
            assertException(M_MUL.expectException(m123_456, r));
        }

        assertException(M_BY_S.expectException(m123_456, v123));
        assertException(M_BY_V.expectException(m123_456, v12));
        assertException(M_BY_M.expectException(m123_456, v123));
    }

    protected static void assertException(final Engine.Result<Throwable> result) {
        System.out.println("Testing " + result.context);
        System.out.println("    " + result.value.getClass().getSimpleName() + ": " + result.value.getMessage());
    }

    public static void main(final String... args) {
        new LinearNaryTest().run();
    }
}
