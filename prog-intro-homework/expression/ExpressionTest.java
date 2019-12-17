package expression;

import java.util.ArrayList;
import java.util.List;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class ExpressionTest extends BaseTest {
    private final List<ToMiniString> prev = new ArrayList<>();
    private final boolean checkMini;

    protected ExpressionTest(final boolean checkMini) {
        this.checkMini = checkMini;
    }

    @Override
    protected void test() {
        handmade();
        generated();
    }

    private void handmade() {
        testExpression("10", "10", new Const(10), x -> 10);
        testExpression("x", "x", new Variable("x"), x -> x);
        testExpression("(x + 2)", "x + 2", new Add(new Variable("x"), new Const(2)), x -> x + 2);
        testExpression("(2 - x)", "2 - x", new Subtract(new Const(2), new Variable("x")), x -> 2 - x);
        testExpression("(3 * x)", "3 * x", new Multiply(new Const(3), new Variable("x")), x -> 3*x);
        testExpression("(x + x)", "x + x", new Add(new Variable("x"), new Variable("x")), x -> x + x);
        testExpression("(x / -2)", "x / -2", new Divide(new Variable("x"), new Const(-2)), x -> -x / 2);
        testExpression("(x + x)", "x + x", new Add(new Variable("x"), new Variable("x")), x -> x + x);
        testExpression("(2 + x)", "2 + x", new Add(new Const(2), new Variable("x")), x -> 2 + x);
        testExpression("(x + 2)", "x + 2", new Add(new Variable("x"), new Const(2)), x -> x + 2);
        testExpression("((1 + 2) + 3)", "1 + 2 + 3", new Add(new Add(new Const(1), new Const(2)), new Const(3)), x -> 6);
        testExpression("(1 + (2 + 3))", "1 + 2 + 3", new Add(new Const(1), new Add(new Const(2), new Const(3))), x -> 6);
        testExpression("((1 - 2) - 3)", "1 - 2 - 3", new Subtract(new Subtract(new Const(1), new Const(2)), new Const(3)), x -> -4);
        testExpression("(1 - (2 - 3))", "1 - (2 - 3)", new Subtract(new Const(1), new Subtract(new Const(2), new Const(3))), x -> 2);
        testExpression("((1 * 2) * 3)", "1 * 2 * 3", new Multiply(new Multiply(new Const(1), new Const(2)), new Const(3)), x -> 6);
        testExpression("(1 * (2 * 3))", "1 * 2 * 3", new Multiply(new Const(1), new Multiply(new Const(2), new Const(3))), x -> 6);
        testExpression("((10 / 2) / 3)", "10 / 2 / 3", new Divide(new Divide(new Const(10), new Const(2)), new Const(3)), x -> 10 / 2 / 3);
        testExpression("(10 / (3 / 2))", "10 / (3 / 2)", new Divide(new Const(10), new Divide(new Const(3), new Const(2))), x -> 10);
        testExpression(
                "((x * x) + ((x - 1) / 10))",
                "x * x + (x - 1) / 10",
                new Add(
                        new Multiply(new Variable("x"), new Variable("x")),
                        new Divide(new Subtract(new Variable("x"), new Const(1)), new Const(10))
                ),
                x -> x * x + (x - 1) / 10
        );
        testExpression("(x * -1000000000)", "x * -1000000000", new Multiply(new Variable("x"), new Const(-1_000_000_000)), x -> x * -1_000_000_000);
        testExpression("(10 / x)", "10 / x", new Divide(new Const(10), new Variable("x")), x -> 10 / x);
        //noinspection PointlessArithmeticExpression
        testExpression("(x / x)", "x / x", new Divide(new Variable("x"), new Variable("x")), x -> x / x);
    }

    @SuppressWarnings("PointlessArithmeticExpression")
    private void generated() {
        final Variable vx = new Variable("x");
        final Const c1 = new Const(1);
        final Const c2 = new Const(2);

        testExpression("(2 + 1)", "2 + 1", new Add(c2, c1), x -> 2 + 1);
        testExpression("(x - 1)", "x - 1", new Subtract(vx, c1), x -> x - 1);
        testExpression("(1 * 2)", "1 * 2", new Multiply(c1, c2), x -> 1 * 2);
        testExpression("(x / 1)", "x / 1", new Divide(vx, c1), x -> x / 1);
        testExpression("(1 + (2 + 1))", "1 + 2 + 1", new Add(c1, new Add(c2, c1)), x -> 1 + 2 + 1);
        testExpression("(x - (x - 1))", "x - (x - 1)", new Subtract(vx, new Subtract(vx, c1)), x -> x - (x - 1));
        testExpression("(2 * (x / 1))", "2 * (x / 1)", new Multiply(c2, new Divide(vx, c1)), x -> 2 * (x / 1));
        testExpression("(2 / (x - 1))", "2 / (x - 1)", new Divide(c2, new Subtract(vx, c1)), x -> 2 / (x - 1));
        testExpression("((1 * 2) + x)", "1 * 2 + x", new Add(new Multiply(c1, c2), vx), x -> 1 * 2 + x);
        testExpression("((x - 1) - 2)", "x - 1 - 2", new Subtract(new Subtract(vx, c1), c2), x -> x - 1 - 2);
        testExpression("((x / 1) * 2)", "x / 1 * 2", new Multiply(new Divide(vx, c1), c2), x -> x / 1 * 2);
        testExpression("((2 + 1) / 1)", "(2 + 1) / 1", new Divide(new Add(c2, c1), c1), x -> (2 + 1) / 1);
        testExpression("(1 + (1 + (2 + 1)))", "1 + 1 + 2 + 1", new Add(c1, new Add(c1, new Add(c2, c1))), x -> 1 + 1 + 2 + 1);
        testExpression("(x - ((1 * 2) + x))", "x - (1 * 2 + x)", new Subtract(vx, new Add(new Multiply(c1, c2), vx)), x -> x - (1 * 2 + x));
        testExpression("(x * (2 / (x - 1)))", "x * (2 / (x - 1))", new Multiply(vx, new Divide(c2, new Subtract(vx, c1))), x -> x * (2 / (x - 1)));
        testExpression("(x / (1 + (2 + 1)))", "x / (1 + 2 + 1)", new Divide(vx, new Add(c1, new Add(c2, c1))), x -> x / (1 + 2 + 1));
        testExpression("((1 * 2) + (2 + 1))", "1 * 2 + 2 + 1", new Add(new Multiply(c1, c2), new Add(c2, c1)), x -> 1 * 2 + 2 + 1);
        testExpression("((2 + 1) - (2 + 1))", "2 + 1 - (2 + 1)", new Subtract(new Add(c2, c1), new Add(c2, c1)), x -> 2 + 1 - (2 + 1));
        testExpression("((x - 1) * (x / 1))", "(x - 1) * (x / 1)", new Multiply(new Subtract(vx, c1), new Divide(vx, c1)), x -> (x - 1) * (x / 1));
        testExpression("((x - 1) / (1 * 2))", "(x - 1) / (1 * 2)", new Divide(new Subtract(vx, c1), new Multiply(c1, c2)), x -> (x - 1) / (1 * 2));
        testExpression("(((x - 1) - 2) + x)", "x - 1 - 2 + x", new Add(new Subtract(new Subtract(vx, c1), c2), vx), x -> x - 1 - 2 + x);
        testExpression("(((1 * 2) + x) - 1)", "1 * 2 + x - 1", new Subtract(new Add(new Multiply(c1, c2), vx), c1), x -> 1 * 2 + x - 1);
        testExpression("(((2 + 1) / 1) * x)", "(2 + 1) / 1 * x", new Multiply(new Divide(new Add(c2, c1), c1), vx), x -> (2 + 1) / 1 * x);
        testExpression("((2 / (x - 1)) / 2)", "2 / (x - 1) / 2", new Divide(new Divide(c2, new Subtract(vx, c1)), c2), x -> 2 / (x - 1) / 2);
    }

    private void testExpression(final String full, final String mini, final Expression actual, final Expression expected) {
        System.out.println("Testing " + full);
        for (int i = 0; i < 10; i++) {
            counter.nextTest();
            assertEquals(String.format("f(%d)", i), evaluate(expected, i), evaluate(actual, i));
            counter.passed();
        }
        checkEqualsAndToString(full, mini, actual);
    }

    protected void checkEqualsAndToString(final String full, final String mini, final ToMiniString expression) {
        final String actualToString = expression.toString();
        checkToString("toString", full, actualToString);
        if (checkMini) {
            checkToString("toMiniString", mini, expression.toMiniString());
        }

        counter.nextTest();
        assertTrue("Equals to this", expression.equals(expression));
        assertTrue("Equals to null", !expression.equals(null));
        counter.passed();

        for (Object prev : prev) {
            counter.nextTest();
            final String prevToString = prev.toString();
            final boolean equals = prevToString.equals(actualToString);
            assertTrue("Equals to " + prevToString, prev.equals(expression) == equals);
            assertTrue("Equals to " + prevToString, expression.equals(prev) == equals);
            assertTrue("Inconsistent hashCode", (prev.hashCode() == expression.hashCode()) == equals);
            counter.passed();
        }

        prev.add(expression);
    }

    private void checkToString(final String method, final String expected, final String actual) {
        counter.nextTest();
        assertTrue(String.format("Invalid %s\n     expected: %s\n       actual: %s", method, expected, actual), expected.equals(actual));
        counter.passed();
    }

    private static Integer evaluate(final Expression expression, final int x) {
        try {
            return expression.evaluate(x);
        } catch (final ArithmeticException e) {
            return null;
        }
    }

    public static void main(final String[] args) {
        new ExpressionTest(mode(args)).run();
    }

    protected static boolean mode(final String[] args) {
        return mode(args, "easy", "hard") == 1;
    }
}
