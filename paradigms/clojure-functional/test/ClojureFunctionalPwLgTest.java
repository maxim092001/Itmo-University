package cljtest.functional;

import cljtest.multi.MultiPwLgTests;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class ClojureFunctionalPwLgTest extends ClojureFunctionalExpressionTest {
    protected ClojureFunctionalPwLgTest(final boolean testMulti) {
        super(new MultiPwLgTests(testMulti));
    }

    public static void main(final String... args) {
        new ClojureFunctionalPwLgTest(mode(args, ClojureFunctionalPwLgTest.class)).run();
    }
}
