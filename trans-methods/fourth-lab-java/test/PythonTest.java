import python.GeneratedParser;
import org.junit.Test;

import java.text.ParseException;

public class PythonTest {
    final String TOTALLY_INCORRECT = "abobabobababbaodbdbbd{{{{eeiieieiei!!!!!!)))))) /?";
    final String SIMPLE_VAR = "x";
    final String NOT_IN = "a not in a";
    final String AND_OR_XOR = "a and a or b xor c";
    final String BRACKETS = "(u or j)";

    @Test(expected = Error.class)
    public void testIncorrectInput() throws ParseException {
        new GeneratedParser(TOTALLY_INCORRECT).e();
    }

    @Test
    public void testSimpleIn() throws ParseException {
        new GeneratedParser(SIMPLE_VAR).e();
    }

    @Test
    public void testNotIn() throws ParseException {
        new GeneratedParser(NOT_IN).e();
    }

    @Test
    public void testManyArgs() throws ParseException {
        new GeneratedParser(AND_OR_XOR).e();
    }

    @Test
    public void testAsteriskTypeArgs() throws ParseException {
        new GeneratedParser(BRACKETS).e();
    }

}
