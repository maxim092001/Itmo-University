package cljtest.linear;

import cljtest.ClojureScript;
import clojure.lang.IPersistentVector;
import jstest.Engine;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class LinearShapelessTest extends LinearBinaryTest {
    public static final ClojureScript.F<?> S_ADD = anyFunction("s+");
    public static final ClojureScript.F<?> S_SUB = anyFunction("s-");
    public static final ClojureScript.F<?> S_MUL = anyFunction("s*");

    static ClojureScript.F<?> anyFunction(final String name) {
        return ClojureScript.function(name, Object.class);
    }

    @Override
    protected void test() {
        super.test();

        final Engine.Result<Number> n10 = number(10);
        final Engine.Result<Number> n20 = number(20);
        assertShapeless(S_ADD.call(n10, n20), 30);
        assertShapeless(S_SUB.call(n10, n20), -10);
        assertShapeless(S_MUL.call(n10, n20), 200);

        final Engine.Result<IPersistentVector> v12 = vector(1, 2);
        final Engine.Result<IPersistentVector> v34 = vector(3, 4);
        assertShapeless(S_ADD.call(v12, v34), v(4, 6));
        assertShapeless(S_SUB.call(v12, v34), v(-2, -2));
        assertShapeless(S_MUL.call(v12, v34), v(3, 8));

        final Engine.Result<IPersistentVector> v1_23 = vector(number(1), vector(2, 3));
        final Engine.Result<IPersistentVector> v4_56 = vector(number(4), vector(5, 6));
        assertShapeless(S_ADD.call(v1_23, v4_56), v(5, v(7, 9)));
        assertShapeless(S_SUB.call(v1_23, v4_56), v(-3, v(-3, -3)));
        assertShapeless(S_MUL.call(v1_23, v4_56), v(4, v(10, 18)));

        final Engine.Result<IPersistentVector> v___1 = vector(vector(vector(number(1))));
        final Engine.Result<IPersistentVector> v___2 = vector(vector(vector(number(2))));
        assertShapeless(S_ADD.call(v___1, v___2), v(v(v(3))));
        assertShapeless(S_SUB.call(v___1, v___2), v(v(v(-1))));
        assertShapeless(S_MUL.call(v___1, v___2), v(v(v(2))));

        final Engine.Result<IPersistentVector> crazy1 = vector(n10, v12, v1_23, v___1);
        final Engine.Result<IPersistentVector> crazy2 = vector(n20, v34, v4_56, v___2);
        assertShapeless(S_ADD.call(crazy1, crazy2), v(30, v(4, 6), v(5, v(7, 9)), v(v(v(3)))));
        assertShapeless(S_SUB.call(crazy1, crazy2), v(-10, v(-2, -2), v(-3, v(-3, -3)), v(v(v(-1)))));
        assertShapeless(S_MUL.call(crazy1, crazy2), v(200, v(3, 8), v(4, v(10, 18)), v(v(v(2)))));
    }

    public static Object v(final Object... values) {
        return values;
    }

    protected void assertShapeless(final Engine.Result<?> result, final Object value) {
        testing(result);
        assertShapeless(result.context, result.value, value);
        counter.passed();
    }

    public static void assertShapeless(final String context, final Object actual, final Object expected) {
        if (expected instanceof Number) {
            assert actual instanceof Number : context + ": Expected number, found " + actual;
            assertEquals(context, EPS, ((Number) expected).doubleValue(), ((Number) actual).doubleValue());
        } else {
            assert  actual instanceof IPersistentVector : context + ": Expected vector, found " + actual;
            final Object[] values = (Object[]) expected;
            final IPersistentVector vector = (IPersistentVector) actual;
            assertEquals(context + ": length", values.length, vector.count());
            for (int i = 0; i < values.length; i++) {
                assertShapeless(context + ":" + i, vector.nth(i), values[i]);
            }
        }
    }

    public static void main(final String... args) {
        new LinearShapelessTest().run();
    }
}
