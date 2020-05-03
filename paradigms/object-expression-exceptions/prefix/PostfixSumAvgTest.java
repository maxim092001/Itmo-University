package jstest.prefix;

import jstest.Language;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class PostfixSumAvgTest extends PrefixSumAvgTest {
    protected PostfixSumAvgTest(final int mode) {
        super(mode, new Language(SUM_ANG_DIALECT, PostfixMixin.DIALECT, new SumAvgTests()), "postfix");
    }

    @Override
    protected void testParsing() {
        PostfixMixin.testErrors(this);
    }

    @Override
    protected String parse(final String expression) {
        return "parsePostfix('" + expression + "')";
    }

    public static void main(final String... args) {
        new PostfixSumAvgTest(prefixMode(args, PostfixSumAvgTest.class)).run();
    }
}
