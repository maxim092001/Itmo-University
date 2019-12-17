package expression;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class TripleExpressionTest extends ExpressionTest {
    public TripleExpressionTest(final boolean checkMini) {
        super(checkMini);
    }

    @Override
    protected void test() {
        super.test();
        handmade();
        generated();
    }

    private void handmade() {
        testExpression("10", "10", new Const(10), (x, y, z) -> 10);
        testExpression("x", "x", new Variable("x"), (x, y, z) -> x);
        testExpression("y", "y", new Variable("y"), (x, y, z) -> y);
        testExpression("z", "z", new Variable("z"), (x, y, z) -> z);
        testExpression("(x + 2)", "x + 2", new Add(new Variable("x"), new Const(2)), (x, y, z) -> x + 2);
        testExpression("(2 - y)", "2 - y", new Subtract(new Const(2), new Variable("y")), (x, y, z) -> 2 - y);
        testExpression("(3 * z)", "3 * z", new Multiply(new Const(3), new Variable("z")), (x, y, z) -> 3 * z);
        testExpression("(x / -2)", "x / -2", new Divide(new Variable("x"), new Const(-2)), (x, y, z) -> -x / 2);
        testExpression("((1 + 2) + 3)", "1 + 2 + 3", new Add(new Add(new Const(1), new Const(2)), new Const(3)), (x, y, z) -> 6);
        testExpression("(1 + (2 + 3))", "1 + 2 + 3", new Add(new Const(1), new Add(new Const(2), new Const(3))), (x, y, z) -> 6);
        testExpression("((1 - 2) - 3)", "1 - 2 - 3", new Subtract(new Subtract(new Const(1), new Const(2)), new Const(3)), (x, y, z) -> -4);
        testExpression("(1 - (2 - 3))", "1 - (2 - 3)", new Subtract(new Const(1), new Subtract(new Const(2), new Const(3))), (x, y, z) -> 2);
        testExpression("((1 * 2) * 3)", "1 * 2 * 3", new Multiply(new Multiply(new Const(1), new Const(2)), new Const(3)), (x, y, z) -> 6);
        testExpression("(1 * (2 * 3))", "1 * 2 * 3", new Multiply(new Const(1), new Multiply(new Const(2), new Const(3))), (x, y, z) -> 6);
        testExpression("((10 / 2) / 3)", "10 / 2 / 3", new Divide(new Divide(new Const(10), new Const(2)), new Const(3)), (x, y, z) -> 10 / 2 / 3);
        testExpression("(10 / (3 / 2))", "10 / (3 / 2)", new Divide(new Const(10), new Divide(new Const(3), new Const(2))), (x, y, z) -> 10);
        testExpression(
                "((x * y) + ((z - 1) / 10))",
                "x * y + (z - 1) / 10", new Add(
                        new Multiply(new Variable("x"), new Variable("y")),
                        new Divide(new Subtract(new Variable("z"), new Const(1)), new Const(10))
                ),
                (x, y, z) -> x * y + (z - 1) / 10
        );
        testExpression("(x + y)", "x + y", new Add(new Variable("x"), new Variable("y")), (x, y, z) -> x + y);
        testExpression("(x + y)", "x + y", new Add(new Variable("x"), new Variable("y")), (x, y, z) -> x + y);
        testExpression("(y + x)", "y + x", new Add(new Variable("y"), new Variable("x")), (x, y, z) -> y + x);
    }

    private void generated() {
        final Variable vx = new Variable("x");
        final Variable vy = new Variable("y");
        final Variable vz = new Variable("z");
        final Const c1 = new Const(1);
        final Const c2 = new Const(2);

        testExpression("(1 + 1)", "1 + 1", new Add(c1, c1), (x, y, z) -> 1 + 1);
        testExpression("(y - x)", "y - x", new Subtract(vy, vx), (x, y, z) -> y - x);
        testExpression("(2 * x)", "2 * x", new Multiply(c2, vx), (x, y, z) -> 2 * x);
        testExpression("(2 / x)", "2 / x", new Divide(c2, vx), (x, y, z) -> 2 / x);
        testExpression("(z + (1 + 1))", "z + 1 + 1", new Add(vz, new Add(c1, c1)), (x, y, z) -> z + 1 + 1);
        testExpression("(2 - (y - x))", "2 - (y - x)", new Subtract(c2, new Subtract(vy, vx)), (x, y, z) -> 2 - (y - x));
        testExpression("(z * (2 / x))", "z * (2 / x)", new Multiply(vz, new Divide(c2, vx)), (x, y, z) -> z * (2 / x));
        testExpression("(z / (y - x))", "z / (y - x)", new Divide(vz, new Subtract(vy, vx)), (x, y, z) -> z / (y - x));
        testExpression("((2 * x) + y)", "2 * x + y", new Add(new Multiply(c2, vx), vy), (x, y, z) -> 2 * x + y);
        testExpression("((y - x) - 2)", "y - x - 2", new Subtract(new Subtract(vy, vx), c2), (x, y, z) -> y - x - 2);
        testExpression("((2 / x) * y)", "2 / x * y", new Multiply(new Divide(c2, vx), vy), (x, y, z) -> 2 / x * y);
        testExpression("((1 + 1) / x)", "(1 + 1) / x", new Divide(new Add(c1, c1), vx), (x, y, z) -> (1 + 1) / x);
        testExpression("(2 + (z + (1 + 1)))", "2 + z + 1 + 1", new Add(c2, new Add(vz, new Add(c1, c1))), (x, y, z) -> 2 + z + 1 + 1);
        testExpression("(1 - ((2 * x) + y))", "1 - (2 * x + y)", new Subtract(c1, new Add(new Multiply(c2, vx), vy)), (x, y, z) -> 1 - (2 * x + y));
        testExpression("(1 * (z / (y - x)))", "1 * (z / (y - x))", new Multiply(c1, new Divide(vz, new Subtract(vy, vx))), (x, y, z) -> 1 * (z / (y - x)));
        testExpression("(z / (z + (1 + 1)))", "z / (z + 1 + 1)", new Divide(vz, new Add(vz, new Add(c1, c1))), (x, y, z) -> z / (z + 1 + 1));
        testExpression("((2 * x) + (1 + 1))", "2 * x + 1 + 1", new Add(new Multiply(c2, vx), new Add(c1, c1)), (x, y, z) -> 2 * x + 1 + 1);
        testExpression("((1 + 1) - (1 + 1))", "1 + 1 - (1 + 1)", new Subtract(new Add(c1, c1), new Add(c1, c1)), (x, y, z) -> 1 + 1 - (1 + 1));
        testExpression("((y - x) * (2 / x))", "(y - x) * (2 / x)", new Multiply(new Subtract(vy, vx), new Divide(c2, vx)), (x, y, z) -> (y - x) * (2 / x));
        testExpression("((y - x) / (2 * x))", "(y - x) / (2 * x)", new Divide(new Subtract(vy, vx), new Multiply(c2, vx)), (x, y, z) -> (y - x) / (2 * x));
        testExpression("(((y - x) - 2) + 1)", "y - x - 2 + 1", new Add(new Subtract(new Subtract(vy, vx), c2), c1), (x, y, z) -> y - x - 2 + 1);
        testExpression("(((2 * x) + y) - z)", "2 * x + y - z", new Subtract(new Add(new Multiply(c2, vx), vy), vz), (x, y, z) -> 2 * x + y - z);
        testExpression("(((1 + 1) / x) * 2)", "(1 + 1) / x * 2", new Multiply(new Divide(new Add(c1, c1), vx), c2), (x, y, z) -> (1 + 1) / x * 2);
        testExpression("((z / (y - x)) / x)", "z / (y - x) / x", new Divide(new Divide(vz, new Subtract(vy, vx)), vx), (x, y, z) -> z / (y - x) / x);
    }

    private void testExpression(final String full, final String mini, final TripleExpression actual, final TripleExpression expected) {
        System.out.println("Testing " + mini);
        for (int i = 0; i < 10; i++) {
            for (int j = 0; j < 10; j++) {
                for (int k = 0; k < 10; k++) {
                    assertEquals(String.format("f(%d, %d, %d)", i, j, k), evaluate(expected, i, j, k), evaluate(actual, i, j, k));
                }
            }
        }
        checkEqualsAndToString(full, mini, actual);
    }

    private Object evaluate(final TripleExpression expected, final int i, final int j, final int k) {
        try {
            return expected.evaluate(i, j, k);
        } catch (Exception e) {
            return e.getClass().getName();
        }
    }

    public static void main(final String[] args) {
        new TripleExpressionTest(mode(args)).run();
    }
}
