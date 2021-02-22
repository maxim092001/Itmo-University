package org.mathoptimization.math_optimization.methods;

import org.mathoptimization.math_optimization.parameters.FibonacciParameters;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

public class FibonacciMethod extends AbstractOptimizationMethod {
    private final List<Double> fibonacciNumbers = new ArrayList<>();

    public FibonacciMethod(final Double left,
                           final Double right,
                           final Function<Double, Double> function,
                           final Double eps) {

        super(left, right, function, eps);
        calculateFibonacciNumbers();
    }

    private void calculateFibonacciNumbers() {
        fibonacciNumbers.add(1.0);
        fibonacciNumbers.add(1.0);
        fibonacciNumbers.add(2.0);

        stepsCount = 0;
        double boundingValue = (right - left) / eps;

        while (boundingValue >= fibonacciNumbers.get(stepsCount + 2)) {
            Double f1 = fibonacciNumbers.get(fibonacciNumbers.size() - 2);
            Double f2 = fibonacciNumbers.get(fibonacciNumbers.size() - 1);
            fibonacciNumbers.add(f1 + f2);
            stepsCount++;
        }
    }

    @Override
    public void calculate() {
        final double startDelta = right - left;
        double x1 = left + fibonacciNumbers.get(stepsCount - 1) / fibonacciNumbers.get(stepsCount + 1) * (right - left);
        double x2 = left + right - x1;
        for (int i = 0; i < stepsCount; i++) {
            parameters.add(new FibonacciParameters(
                    left,
                    right,
                    (right + left) / 2.0,
                    function.apply((right + left) / 2.0)
            ));
            if (function.apply(x1) < function.apply(x2)) {
                right = x2;
            } else {
                left = x1;
            }
            x1 = left + fibonacciNumbers.get(stepsCount - (i + 1)) / fibonacciNumbers.get(stepsCount + 1) * startDelta;
            x2 = left + fibonacciNumbers.get(stepsCount - (i + 1) + 1) / fibonacciNumbers.get(stepsCount + 1) * startDelta;
        }
        minArgument = (x1 + x2) / 2.0;
        minValue = function.apply(minArgument);
    }

}
