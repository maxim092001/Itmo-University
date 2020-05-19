package cljtest.functional;

import cljtest.multi.MultiSumexpSoftmaxTests;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class ClojureFunctionalSumexpSoftmaxTest extends ClojureFunctionalExpressionTest {
    protected ClojureFunctionalSumexpSoftmaxTest(final boolean testMulti) {
        super(new MultiSumexpSoftmaxTests(testMulti));
    }

    public static void main(final String... args) {
        new ClojureFunctionalSumexpSoftmaxTest(mode(args, ClojureFunctionalSumexpSoftmaxTest.class)).run();
    }
}
