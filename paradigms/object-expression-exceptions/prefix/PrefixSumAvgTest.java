package jstest.prefix;

import jstest.ArithmeticTests;
import jstest.Language;
import jstest.object.ObjectExpressionTest;

import java.util.List;
import java.util.Random;
import java.util.function.Supplier;
import java.util.stream.Stream;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class PrefixSumAvgTest extends PrefixParsingErrorTest {
    private static final Random RANDOM = new Random(343243543059325L);
    public static final Dialect SUM_ANG_DIALECT = ObjectExpressionTest.ARITHMETIC_DIALECT.copy()
            .rename("sum", "Sum")
            .rename("avg", "Avg");

    public static class SumAvgTests extends ArithmeticTests {{
        any("sum", 3, PrefixSumAvgTest::sum);
        any("avg", 5, args -> sum(args) / args.size());

        tests(
                f("sum", vx),
                f("sum", vx, vy),
                f("sum", vx, vy, vz),
                f("sum", vx, vy, vz, c(3), c(5)),
                f("sum"),
                f("sum", f("-", vx, vy)),
                f("sum", f("+", vx, vy)),
                f("sum", f("negate", vz), f("/", vx, vy)),
                f("sum", f("negate", vz), f("sum", vx, vy)),
                f("avg", vx),
                f("avg", vx, vy),
                f("avg", vx, vy, vz),
                f("avg", vx, vy, vz, c(3), c(5)),
                f("avg"),
                f("avg", f("negate", vz), f("avg", vx, vy))
        );
        final Supplier<AbstractExpression> generator = () -> random(vx, vy, vz, c(RANDOM.nextInt(10)));
        for (int i = 1; i < 10; i++) {
            final AbstractExpression[] args = Stream.generate(generator).limit(i).toArray(AbstractExpression[]::new);
            tests(
                    f("sum", args),
                    f("avg", args)
            );
        }
    }}

    private static double sum(final List<Double> args) {
        return args.stream().mapToDouble(a -> a).sum();
    }

    @SafeVarargs
    private static <T> T random(final T... values) {
        return values[RANDOM.nextInt(values.length)];
    }

    protected PrefixSumAvgTest(final int mode, final Language language, final String toString) {
        super(mode, language, toString);
        insertions = "()+*/@ABC";
    }

    public static void main(final String... args) {
        test(PrefixSumAvgTest.class, PrefixSumAvgTest::new, new SumAvgTests(), args, SUM_ANG_DIALECT, "prefix");
    }
}
