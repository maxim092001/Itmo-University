package jstest.prefix;

import jstest.ArithmeticTests;
import jstest.Language;
import jstest.object.ObjectExpressionTest;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class PrefixSinhCoshTest extends PrefixParsingErrorTest {
    public static final Dialect SINH_COSH_DIALECT = ObjectExpressionTest.ARITHMETIC_DIALECT.copy()
            .rename("sinh", "Sinh")
            .rename("cosh", "Cosh");

    public static class SinhCoshTests extends ArithmeticTests {{
        unary("sinh", Math::sinh);
        unary("cosh", Math::cosh);
        tests(
                f("sinh", f("-", vx, vy)),
                f("cosh", f("+", vx, vy)),
                f("cosh", f("/", f("sinh", vz), f("+", vx, vy))),
                f("+", f("cosh", f("sinh", f("+", vx, c(10)))), f("*", vz, f("*", vy, f("cosh", c(0)))))
        );
    }}

    protected PrefixSinhCoshTest(final int mode, final Language language, final String toString) {
        super(mode, language, toString);
    }

    public static void main(final String... args) {
        test(PrefixSinhCoshTest.class, PrefixSinhCoshTest::new, new SinhCoshTests(), args, SINH_COSH_DIALECT, "prefix");
    }
}
