package jstest.prefix;

import expression.BaseTest;
import jstest.*;
import jstest.object.ObjectExpressionTest;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class PrefixParserTest extends ObjectExpressionTest {
    public String insertions = "xyz()+*/@ABC";
    public static final Dialect PREFIX = dialect(
            "%s",
            "%s",
            (op, args) -> "(" + op + " " + String.join(" ", args) + ")"
    );

    public PrefixParserTest(final int mode, final Language language, final String toString) {
        super(mode, language);
        engine.toStringMethod = toString;
    }

    @Override
    protected String parse(final String expression) {
        return "parsePrefix('" + expression + "')";
    }

    @Override
    protected void test(final String parsed, final String unparsed) {
        super.test(parsed, unparsed);
        super.test(removeSpaces(parsed), unparsed);

        for (int i = 0; i < 1 + Math.min(10, 200 / unparsed.length()); i++) {
            final int index = randomInt(unparsed.length());
            final char c = unparsed.charAt(index);
            if (!Character.isDigit(c) && !Character.isWhitespace(c) && "-hxyz".indexOf(c) == -1){
                counter.nextTest();
                assertParsingError(unparsed.substring(0, index), "<SYMBOL REMOVED>", unparsed.substring(index + 1));
                counter.passed();
            }
            final char newC = insertions.charAt(randomInt(insertions.length()));
            if (!Character.isDigit(c) && c != '-') {
                counter.nextTest();
                assertParsingError(unparsed.substring(0, index), "<SYMBOL INSERTED -->", newC + unparsed.substring(index));
                counter.passed();
            }
        }
    }

    private static String removeSpaces(final String expression) {
        return expression.replace(" (", "(").replace(") ", ")");
    }

    protected String assertParsingError(final String prefix, final String comment, final String suffix) {
        try {
            engine.parse(parse(prefix + suffix));
            throw new AssertionError("Parsing error expected for " + prefix + comment + suffix);
        } catch (final EngineException e) {
            return e.getCause().getMessage();
        }
    }

    public static void main(final String... args) {
        test(PrefixParserTest.class, PrefixParserTest::new, new ArithmeticTests(), args, ARITHMETIC_DIALECT, "prefix");
    }

    public static <T extends BaseTest> void test(final Class<T> type, final Constructor<T> cons, final AbstractTests tests, final String[] args, final Dialect parsed, final String toString) {
        cons.create(prefixMode(args, type), new Language(parsed, PREFIX, tests), toString).run();
    }

    protected static <T extends BaseTest> int prefixMode(final String[] args, final Class<T> type) {
        return mode(args, type, "easy", "hard") + 1;
    }

    interface Constructor<T> {
        T create(int mode, Language language, String toString);
    }
}
