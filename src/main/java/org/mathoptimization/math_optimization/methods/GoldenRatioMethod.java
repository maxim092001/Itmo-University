package org.mathoptimization.math_optimization.methods;

import org.mathoptimization.math_optimization.parameters.GoldenRatioParameters;

import java.util.function.Function;

public class GoldenRatioMethod extends AbstractOptimizationMethod {
    private final double tau = (Math.sqrt(5.0) - 1.0) / 2.0;

    public GoldenRatioMethod(final Double left,
                             final Double right,
                             final Function<Double, Double> function,
                             final Double eps) {
        super(left, right, function, eps);
    }

    @Override
    public void calculate() {
        parameters.clear();
        double x1 = left + (3.0 - Math.sqrt(5.0)) / 2.0 * intervalLength();
        double x2 = left + (Math.sqrt(5.0) - 1.0) / 2.0 * intervalLength();
        double currentEps = intervalLength() / 2;
        while (currentEps > eps) {
            parameters.add(new GoldenRatioParameters(
                    left,
                    right,
                    (right + left) / 2.0,
                    function.apply((right + left) / 2.0)
            ));
            currentEps *= tau;
            if (function.apply(x1) < function.apply(x2)) {
                right = x2;
                x2 = x1;
                x1 = right - tau * intervalLength();
            } else {
                left = x1;
                x1 = x2;
                x2 = right - tau * intervalLength();
            }
        }
        minArgument = (right + left) / 2.0;
        minValue = function.apply(minArgument);
    }
}
