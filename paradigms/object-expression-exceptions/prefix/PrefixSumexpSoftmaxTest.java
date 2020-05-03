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
public class PrefixSumexpSoftmaxTest extends PrefixParsingErrorTest {
    private static final Random RANDOM = new Random(343243543059325L);
    public static final Dialect SUMEXP_SOFTMAX_DIALECT = ObjectExpressionTest.ARITHMETIC_DIALECT.copy()
            .rename("sumexp", "Sumexp")
            .rename("softmax", "Softmax");

    public static class SumexpSoftmaxTests extends ArithmeticTests {{
        any("sumexp", 3, PrefixSumexpSoftmaxTest::sumexp);
        any("softmax", 5, args -> Math.exp(args.get(0)) / sumexp(args));

        tests(
                f("sumexp", vx),
                f("sumexp", vx, vy),
                f("sumexp", vx, vy, vz),
                f("sumexp", vx, vy, vz, c(3), c(5)),
                f("sumexp"),
                f("sumexp", f("-", vx, vy)),
                f("sumexp", f("+", vx, vy)),
                f("sumexp", f("negate", vz), f("/", vx, vy)),
                f("softmax", vx),
                f("softmax", vx, vy),
                f("softmax", vx, vy, vz),
                f("softmax", vx, vy, vz, c(3), c(5)),
                f("softmax", f("negate", vz), f("softmax", vx, vy))
        );
        final Supplier<AbstractExpression> generator = () -> random(vx, vy, vz, c(RANDOM.nextInt(10)));
        for (int i = 1; i < 10; i++) {
            final AbstractExpression[] args = Stream.generate(generator).limit(i).toArray(AbstractExpression[]::new);
            tests(
                    f("sumexp", args),
                    f("softmax", args)
            );
        }
    }}

    private static double sumexp(final List<Double> args) {
        return args.stream().mapToDouble(Math::exp).sum();
    }

    @SafeVarargs
    private static <T> T random(final T... values) {
        return values[RANDOM.nextInt(values.length)];
    }

    protected PrefixSumexpSoftmaxTest(final int mode, final Language language, final String toString) {
        super(mode, language, toString);
        insertions = "()+*/@ABC";
    }

    public static void main(final String... args) {
        test(PrefixSumexpSoftmaxTest.class, PrefixSumexpSoftmaxTest::new, new SumexpSoftmaxTests(), args, SUMEXP_SOFTMAX_DIALECT, "prefix");
    }
}
