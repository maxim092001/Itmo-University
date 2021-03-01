package org.mathoptimization.math_optimization;

import org.junit.Before;
import org.junit.Test;
import org.mathoptimization.math_optimization.services.FunctionService;
import java.util.function.Function;
import static org.junit.Assert.*;

public class FunctionServiceTests {
    private FunctionService functionService;

    @Before
    public void before() {
        functionService = new FunctionService();
    }

    @Test
    public void singleLineTest() throws Exception {
        String sqrStr = "pow(x, 2)";
        Function<Double, Double> sqr = functionService.parse(sqrStr);

        assertTrue(equals(4.0, sqr.apply(2.0)));
        assertTrue(equals(81.0, sqr.apply(9.0)));
        assertTrue(equals(81.0, sqr.apply(-9.0)));
        assertTrue(equals(0.25, sqr.apply(0.5)));
        assertTrue(equals(16.0, sqr.andThen(sqr).apply(2.0)));
    }

    @Test(expected = FunctionService.FunctionServiceException.class)
    public void compileErrorTest() throws Exception {
        String s = "";
        functionService.parse(s);
    }

    private static boolean equals(double a, double b) {
        return Math.abs(a - b) < 1e-8;
    }
}
