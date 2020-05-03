package jstest.prefix;

import jstest.Language;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class PostfixMeanVarTest extends PrefixMeanVarTest {
    protected PostfixMeanVarTest(final int mode) {
        super(mode, new Language(MEAN_VAR_DIALECT, PostfixMixin.DIALECT, new MeanVarTests()), "postfix");
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
        new PostfixMeanVarTest(prefixMode(args, PostfixMeanVarTest.class)).run();
    }
}
