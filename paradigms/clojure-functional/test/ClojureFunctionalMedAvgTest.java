package cljtest.functional;

import cljtest.multi.MultiMedAvgTests;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class ClojureFunctionalMedAvgTest extends ClojureFunctionalExpressionTest {
    protected ClojureFunctionalMedAvgTest(final boolean testMulti) {
        super(new MultiMedAvgTests(testMulti));
    }

    public static void main(final String... args) {
        new ClojureFunctionalMedAvgTest(mode(args, ClojureFunctionalMedAvgTest.class)).run();
    }
}
