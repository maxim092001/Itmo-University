import org.junit.Test;

import java.text.ParseException;

public class ParserTest {

    private static final Parser parser = new Parser();
    private static final String UNKNOWN = "Facebook -><><><_____-----<<<<<>>>>>>>>          <<><><><><><>Meta?";
    private static final String DOUBLE_IN = "x in      in y";
    private static final String NOT_IN = "x          not in y";
    private static final String NOT_IN_WITH_PARENTHESES = "x not in (y)";
    private static final String VAR_IN_EXPRESSION = "x in (a             and b)";
    private static final String EXPRESSION_IN_VAR = "(a and b) in (x)";
    private static final String SIMPLE_EXPRESSION = "a and b or          c and (x or y)";
    private static final String EMPTY_PARENTHESES = "()";
    private static final String WRONG_PARENTHESES = "(a          and b";

    @Test
    public void test() throws ParseException {
        parser.parse("(a or b) and c");
    }

    @Test(expected = ParseException.class)
    public void testDoubleIn() throws ParseException {
        parser.parse(DOUBLE_IN);
    }

    @Test
    public void testNotIn() throws ParseException {
        parser.parse(NOT_IN);
    }

    @Test(expected = ParseException.class)
    public void notInWithParentheses() throws ParseException {
        parser.parse(NOT_IN_WITH_PARENTHESES);
    }

    @Test(expected = ParseException.class)
    public void varInExpression() throws ParseException {
        parser.parse(VAR_IN_EXPRESSION);
    }

    @Test(expected = ParseException.class)
    public void expressionInVar() throws ParseException {
        parser.parse(EXPRESSION_IN_VAR);
    }

    @Test
    public void simpleExpression() throws ParseException {
        parser.parse(SIMPLE_EXPRESSION);
    }


    @Test(expected = ParseException.class)
    public void emptyParentheses() throws ParseException {
        parser.parse(EMPTY_PARENTHESES);
    }

    @Test(expected = ParseException.class)
    public void wrongParentheses() throws ParseException {
        parser.parse(WRONG_PARENTHESES);
    }


    @Test(expected = ParseException.class)
    public void unknown() throws ParseException {
        parser.parse(UNKNOWN);
    }

}
