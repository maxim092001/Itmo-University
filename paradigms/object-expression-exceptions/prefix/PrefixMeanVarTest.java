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
public class PrefixMeanVarTest extends PrefixParsingErrorTest {
    private static final Random RANDOM = new Random(343243543059325L);
    public static final Dialect MEAN_VAR_DIALECT = ObjectExpressionTest.ARITHMETIC_DIALECT.copy()
            .rename("mean", "Mean")
            .rename("var", "Var");

    public static class MeanVarTests extends ArithmeticTests {{
        any("mean", 3, PrefixMeanVarTest::mean);
        any("var", 5, PrefixMeanVarTest::var);

        tests(
                f("mean", vx),
                f("mean", vx, vy),
                f("mean", vx, vy, vz),
                f("mean", vx, vy, vz, c(3), c(5)),
                f("mean"),
                f("mean", f("-", vx, vy)),
                f("mean", f("+", vx, vy)),
                f("mean", f("negate", vz), f("/", vx, vy)),
                f("mean", f("negate", vz), f("mean", vx, vy)),
                f("var", vx),
                f("var", vx, vy),
                f("var", vx, vy, vz),
                f("var", vx, vy, vz, c(3), c(5)),
                f("var"),
                f("var", f("negate", vz), f("var", vx, vy))
        );
        final Supplier<AbstractExpression> generator = () -> random(vx, vy, vz, c(RANDOM.nextInt(10)));
        for (int i = 1; i < 10; i++) {
            final AbstractExpression[] args = Stream.generate(generator).limit(i).toArray(AbstractExpression[]::new);
            tests(
                    f("mean", args),
                    f("var", args)
            );
        }
    }}

    private static double var(final List<Double> args) {
        final double mean = mean(args);
        return args.stream().mapToDouble(a -> a - mean).map(a -> a * a).sum() / args.size();
    }

    private static double mean(final List<Double> args) {
        return args.stream().mapToDouble(a -> a).sum() / args.size();
    }

    @SafeVarargs
    private static <T> T random(final T... values) {
        return values[RANDOM.nextInt(values.length)];
    }

    protected PrefixMeanVarTest(final int mode, final Language language, final String toString) {
        super(mode, language, toString);
        insertions = "()+*/@ABC";
    }

    public static void main(final String... args) {
        test(PrefixMeanVarTest.class, PrefixMeanVarTest::new, new MeanVarTests(), args, MEAN_VAR_DIALECT, "prefix");
    }
}
