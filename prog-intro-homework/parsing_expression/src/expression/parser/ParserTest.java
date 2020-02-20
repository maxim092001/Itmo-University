package expression.parser;

import expression.BaseTest;
import expression.Either;
import expression.TripleExpression;

import java.util.ArrayList;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.LongBinaryOperator;
import java.util.function.LongSupplier;
import java.util.function.LongUnaryOperator;

/**
 * @author Niyaz Nigmatullin
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class ParserTest extends BaseTest {
    private final static int D = 5;

    private final static List<Integer> TEST_VALUES = new ArrayList<>();
    static {
        addRange(TEST_VALUES, D, D);
        addRange(TEST_VALUES, D, -D);
    }


    protected final List<Op<LongUnaryOperator>> unary = new ArrayList<>();
    protected final List<List<Op<LongBinaryOperator>>> levels = new ArrayList<>();
    protected List<Op<TExpression>> tests;

    protected ParserTest() {
        unary.add(op("-", a -> -a));

        //noinspection Convert2MethodRef
        levels.add(list(
                op("+", (a, b) -> a + b),
                op("-", (a, b) -> a - b)
        ));
        levels.add(list(
                op("*", (a, b) -> a * b),
                op("/", (a, b) -> b == 0 ? error(DBZ) : a / b)
        ));

        tests = list(
                op("10", (x, y, z) -> 10L),
                op("x", (x, y, z) -> x),
                op("y", (x, y, z) -> y),
                op("z", (x, y, z) -> z),
                op("x+2", (x, y, z) -> x + 2),
                op("2-y", (x, y, z) -> 2 - y),
                op("  3*  z  ", (x, y, z) -> 3 * z),
                op("x/  -  2", (x, y, z) -> -x / 2),
                op("x*y+(z-1   )/10", (x, y, z) -> x * y + (int) (z - 1) / 10),
                op("-(-(-\t\t-5 + 16   *x*y) + 1 * z) -(((-11)))", (x, y, z) -> -(-(5 + 16 * x * y) + z) + 11),
                op("" + Integer.MAX_VALUE, (x, y, z) -> (long) Integer.MAX_VALUE),
                op("" + Integer.MIN_VALUE, (x, y, z) -> (long) Integer.MIN_VALUE),
                op("x--y--z", (x, y, z) -> x + y + z),
                op("((2+2))-0/(--2)*555", (x, y, z) -> 4L),
                op("x-x+y-y+z-(z)", (x, y, z) -> 0L),
                op("(".repeat(500) + "x + y + (-10*-z)" + ")".repeat(500), (x, y, z) -> x + y + 10 * z),
                op("x / y / z", (x, y, z) -> y == 0 || z == 0 ? error(DBZ) : (int) x / (int) y / z)
        );
    }

    public static void main(final String[] args) {
        new ParserTest().run();
    }

    @Override
    protected void test() {
        for (final Op<TExpression> test : tests) {
            System.out.println("Testing: " + test.name);
            final TripleExpression expression = parse(test.name, true);
            for (final Integer x : TEST_VALUES) {
                for (final Integer y : TEST_VALUES) {
                    for (final Integer z : TEST_VALUES) {
                        check(new int[]{x, y, z}, expression, eval(() -> test.f.evaluate(x, y, z)));
                    }
                }
            }
        }

        testRandom(1, 2000, (v, i) -> generate(v, i / 5 + 2));
        testRandom(2, 777, (v, i) -> genExpression(1, i / 25 / levels.size() + 1, v, 0));
    }

    private Either<Reason, Integer> eval(final LongSupplier supplier) {
        try {
            return lift(supplier.getAsLong());
        } catch (final ExpException e) {
            return Either.left(e.reason);
        }
    }

    protected TripleExpression parse(final String expression, final boolean reparse) {
        try {
            final ExpressionParser parser = new ExpressionParser();
            if (reparse) {
                counter.nextTest();
                parser.parse(expression);
                counter.passed();
            }
            counter.nextTest();
            final TripleExpression result = parser.parse(expression);
            counter.passed();
            return result;
        } catch (final Exception e) {
            throw new AssertionError("Parser failed", e);
        }
    }

    protected void testRandom(final int seq, final int n, final BiFunction<int[], Integer, Test> f) {
        System.out.println("Testing random tests #" + seq);
        for (int i = 0; i < n; i++) {
            if (i % 100 == 0) {
                System.out.println("Completed " + i + " out of " + n);
            }
            final int[] vars = new int[]{random.nextInt(), random.nextInt(), random.nextInt()};

            final Test test = f.apply(vars, i);
            try {
                check(vars, parse(test.expr, false), test.answer);
            } catch (final Throwable e) {
                System.out.println("Failed test: " + test.expr);
                throw e;
            }
        }
    }

    private void check(final int[] vars, final TripleExpression expression, final Either<Reason, Integer> answer) {
        counter.nextTest();
        try {
            final int actual = expression.evaluate(vars[0], vars[1], vars[2]);
            assert answer.isRight() : String.format("Error expected for x=%d, y=%d, z=%d", vars[0], vars[1], vars[2]);
            assertEquals(String.format("f(%d, %d, %d)\n%s", vars[0], vars[1], vars[2], expression), answer.getRight(), actual);
        } catch (final Exception e) {
            if (answer.isRight()) {
                throw new AssertionError(String.format("No error expected for x=%d, y=%d, z=%d", vars[0], vars[1], vars[2]), e);
            }
        }
        counter.passed();
    }

    private Test generate(final int[] vars, final int depth) {
        if (depth == 0) {
            return constOrVariable(vars);
        }
        final int operator = randomInt(6);
        if (operator <= 0) {
            return genP(vars, depth);
        } else if (operator <= 1) {
            return unary(genP(vars, depth));
        } else {
            return binary(random(levels), genP(vars, depth), genP(vars, depth));
        }
    }

    private Test genP(final int[] vars, final int depth) {
        return p(generate(vars, randomInt(depth)));
    }

    private Test constOrVariable(final int[] vars) {
        if (random.nextBoolean()) {
            final int id = randomInt(3);
            return new Test("xyz".charAt(id) + "", Either.right(vars[id]));
        } else {
            final int value = random.nextInt();
            return new Test(value + "", Either.right(value));
        }
    }

    private Test genExpression(final int depth, final int coefficient, final int[] vars, final int level) {
        if (level == levels.size()) {
            return genFactor(depth, coefficient, vars);
        } else if (makeNewBranch(depth, coefficient)) {
            return binary(levels.get(level), genExpression(depth + 1, coefficient, vars, level), genExpression(depth, coefficient, vars, level + 1));
        } else {
            return genExpression(depth, coefficient, vars, level + 1);
        }
    }

    private Test genFactor(final int depth, final int coefficient, final int[] vars) {
        if (makeNewBranch(depth, coefficient)) {
            return unary(genFactor(depth + 1, coefficient, vars));
        } else {
            return genValue(depth, coefficient, vars);
        }
    }

    private static Test p(final Test t) {
        return new Test("("  + t.expr + ")", t.answer);
    }

    private Test binary(final List<Op<LongBinaryOperator>> ops, final Test t1, final Test t2) {
        final Op<LongBinaryOperator> op = random(ops);
        return new Test(
                t1.expr + " " + op.name + " " + t2.expr,
                t1.answer.flatMapRight(a -> t2.answer.flatMapRight(b -> eval(() -> op.f.applyAsLong(a, b))))
        );
    }

    private Test unary(final Test arg) {
        final Op<LongUnaryOperator> op = random(unary);
        return new Test(op.name + " " + arg.expr, arg.answer.flatMapRight(a -> eval(() -> op.f.applyAsLong(a))));
    }

    private Test genValue(final int depth, final int coefficient, final int[] vars) {
        if (makeNewBranch(depth, coefficient)) {
            return p(genExpression(depth + 1, coefficient, vars, 0));
        } else {
            return constOrVariable(vars);
        }
    }

    private boolean makeNewBranch(final int depth, final int coefficient) {
        return randomInt(depth + coefficient) < coefficient;
    }

    protected Either<Reason, Integer> lift(final long value) {
        return Either.right((int) value);
    }

    protected static class Test {
        final String expr;
        final Either<Reason, Integer> answer;

        Test(final String expr, final Either<Reason, Integer> answer) {
            this.expr = expr;
            this.answer = answer;
        }
    }

    protected static class Reason {
        private final String description;

        public Reason(final String description) {
            this.description = description;
        }
    }

    public static final Reason DBZ = new Reason("Division by zero");

    public interface TExpression {
        long evaluate(long x, long y, long z);
    }

    public static class ExpException extends RuntimeException {
        private final Reason reason;

        public ExpException(final Reason reason) {
            super(reason.description);
            this.reason = reason;
        }
    }

    public static long error(final Reason reason) {
        throw new ExpException(reason);
    }
}
