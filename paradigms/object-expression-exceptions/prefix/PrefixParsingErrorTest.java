package jstest.prefix;

import jstest.Language;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class PrefixParsingErrorTest extends PrefixParserTest {
    public PrefixParsingErrorTest(final int mode, final Language language, final String toString) {
        super(mode, language, toString);
    }

    @Override
    protected final void test() {
        super.test();

        testParsing();
    }

    protected void testParsing() {
        printParsingError("Empty input", "");
        printParsingError("Unknown variable", "a");
        printParsingError("Invalid number", "-a");
        printParsingError("Missing )", "(* z (+ x y)");
        printParsingError("Unknown operation", "(@@  x y)");
        printParsingError("Excessive info", "(+ x y) x");
        printParsingError("Empty op", "()");
        printParsingError("Invalid unary (0 args)", "(negate)");
        printParsingError("Invalid unary (2 args)", "(negate x y)");
        printParsingError("Invalid binary (0 args)", "(+)");
        printParsingError("Invalid binary (1 args)", "(+ x)");
        printParsingError("Invalid binary (3 args)", "(+ x y z)");
        printParsingError("Variable op (0 args)", "(x)");
        printParsingError("Variable op (1 args)", "(x 1)");
        printParsingError("Variable op (2 args)", "(x 1 2)");
        printParsingError("Const op (0 args)", "(0)");
        printParsingError("Const op (1 args)", "(0 1)");
        printParsingError("Const op (2 args)", "(0 1 2)");
    }

    protected final void printParsingError(final String description, final String input) {
        final String message = assertParsingError(input, "", "");
        final int index = message.lastIndexOf("in <eval>");

        System.err.format("%-25s: %s%n", description, message.substring(0, index > 0 ? index : message.length()));
    }
}
