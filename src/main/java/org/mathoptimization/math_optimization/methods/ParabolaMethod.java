package org.mathoptimization.math_optimization.methods;

import org.mathoptimization.math_optimization.parameters.ParabolaParameters;
import org.mathoptimization.math_optimization.parameters.Parameters;

import java.util.function.Function;

public class ParabolaMethod extends AbstractOptimizationMethod {

    public ParabolaMethod(final Double left,
                          final Double right,
                          final Function<Double, Double> function,
                          final Double eps) {
        super(left, right, function, eps);
    }

    private Double findX2(Double x1, Double x3) {
        return Math.random() * (x3 - x1) + x1;
    }


    @Override
    public void run() {
        Double x1 = left, x3 = right;

        while (x3 - x1 > eps) {
            Double x2 = findX2(x1, x3);
            Double f1 = function.apply(x1);
            Double f2 = function.apply(x2);
            Double f3 = function.apply(x3);

            Double a0 = f1;
            Double a1 = (f2 - f1) / (x2 - x1);
            Double a2 = ((f3 - f1) / (x3 - x1) - (f2 - f1) / (x2 - x1)) / (x3 - x2);

            Double xMin = (x1 + x2 - a1 / a2) / 2.0;
            Double fxMin = function.apply(xMin);

            Parameters step = new ParabolaParameters(left, right,
                    f1, x1,
                    a0, a1, a2,
                    xMin, fxMin,
                    f2, f3,
                    x2, x3);

            parameters.add(step);

            if (fxMin < f2) {
                if (xMin < x2) {
                    x3 = x2;
                } else {
                    x1 = x2;
                }
            } else {
                if (xMin < x2) {
                    x1 = xMin;
                } else {
                    x3 = xMin;
                }
            }
        }

        minArgument = (x3 + x1) / 2.0;
        minValue = function.apply(minArgument);

    }
}
