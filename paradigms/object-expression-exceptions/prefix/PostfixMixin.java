package jstest.prefix;

import jstest.BaseJavascriptTest;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class PostfixMixin {
    public static final BaseJavascriptTest.Dialect DIALECT = BaseJavascriptTest.dialect(
            "%s",
            "%s",
            (op, args) -> "(" + String.join(" ", args) + " " + op + ")"
    );

    public static void testErrors(final PrefixParsingErrorTest test) {
        test.printParsingError("Empty input", "");
        test.printParsingError("Unknown variable", "a");
        test.printParsingError("Invalid number", "-a");
        test.printParsingError("Missing )", "(z (x y +) *");
        test.printParsingError("Missing (", "z (x y +) *)");
        test.printParsingError("Unknown operation", "( x y @@)");
        test.printParsingError("Excessive info", "(x y +) x");
        test.printParsingError("Empty op", "()");
        test.printParsingError("Invalid unary (0 args)", "(negate)");
        test.printParsingError("Invalid unary (2 args)", "(x y negate)");
        test.printParsingError("Invalid binary (0 args)", "(+)");
        test.printParsingError("Invalid binary (1 args)", "(x +)");
        test.printParsingError("Invalid binary (3 args)", "(x y z +)");
        test.printParsingError("Variable op (0 args)", "(x)");
        test.printParsingError("Variable op (1 args)", "(1 x)");
        test.printParsingError("Variable op (2 args)", "(1 2 x)");
        test.printParsingError("Const op (0 args)", "(0)");
        test.printParsingError("Const op (1 args)", "(0 1)");
        test.printParsingError("Const op (2 args)", "(0 1 2)");
    }
}
