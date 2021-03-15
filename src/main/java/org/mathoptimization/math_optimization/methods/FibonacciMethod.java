package org.mathoptimization.math_optimization.methods;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.mathoptimization.math_optimization.parameters.FibonacciParameters;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

/**
 * Fibonacci optimization method.
 */
@EqualsAndHashCode(callSuper = true)
@Data
@NoArgsConstructor
public class FibonacciMethod extends AbstractOptimizationMethod {
    private final List<Double> fibonacciNumbers = new ArrayList<>();

    public FibonacciMethod(final Double left,
                           final Double right,
                           final Function<Double, Double> function,
                           final Double eps) {
        super(left, right, function, eps);
    }

    @Override
    public void validate() {
        if (left == null || right == null || eps == null ||
                 eps < 0 || Math.abs(eps) < 1e-9 || left >= right || function == null) {
            throw new RuntimeException();
        }
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
        calculateFibonacciNumbers();
        validate();
        final double startDelta = right - left;
        double x1 = left + fibonacciNumbers.get(stepsCount - 1) / fibonacciNumbers.get(stepsCount + 1) * startDelta;
        double x2 = left + right - x1;
        double f1 = function.apply(x1);
        double f2 = function.apply(x2);
        for (int i = 1; i < stepsCount - 1; i++) {
            if (f1 > f2) {
                left = x1;
                x1 = x2;
                f1 = f2;
                x2 = left + fibonacciNumbers.get(stepsCount - i - 1)/ fibonacciNumbers.get(stepsCount - i) * (right - left);
                f2 = function.apply(x2);
            } else {
                right = x2;
                x2 = x1;
                f2 = f1;
                x1 = left + fibonacciNumbers.get(stepsCount - i - 2)/ fibonacciNumbers.get(stepsCount - i) * (right - left);
                f1 = function.apply(x1);
            }
        }
        minArgument = (x1 + x2) / 2.0;
        minValue = function.apply(minArgument);
    }

}
