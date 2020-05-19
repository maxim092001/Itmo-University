package cljtest.functional;

import cljtest.multi.MultiMinMaxTests;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class ClojureFunctionalMinMaxTest extends ClojureFunctionalExpressionTest {
    protected ClojureFunctionalMinMaxTest(final boolean testMulti) {
        super(new MultiMinMaxTests(testMulti));
    }

    public static void main(final String... args) {
        new ClojureFunctionalMinMaxTest(mode(args, ClojureFunctionalMinMaxTest.class)).run();
    }
}
