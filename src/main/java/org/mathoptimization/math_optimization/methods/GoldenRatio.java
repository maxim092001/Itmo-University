package org.mathoptimization.math_optimization.methods;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

public class GoldenRatio implements OptimizationMethod {
    private Function<Double, Double> function;
    private Double left;
    private Double right;
    private Double eps;
    private int stepsCount;
    private Double minValue;
    private Double minArgument;
    private final List<MethodStep> steps = new ArrayList<>();
    private final double tau = (Math.sqrt(5.0) - 1.0) / 2.0;

    @Override
    public OptimizationMethod setFunction(final Function<Double, Double> function) {
        this.function = function;
        return this;
    }

    @Override
    public OptimizationMethod setInterval(final Double left, final Double right) {
        this.left = left;
        this.right = right;
        return this;
    }

    @Override
    public OptimizationMethod setEpsilon(final Double eps) {
        this.eps = eps;
        return this;
    }

    @Override
    public OptimizationMethod setStepsCount(final int stepsCount) {
        this.stepsCount = stepsCount;
        return this;
    }

    @Override
    public void run() {
        double x1 = left + (3.0 - Math.sqrt(5.0)) / 2.0 * (right - left);
        double x2 = left + (Math.sqrt(5.0) - 1.0) / 2.0 * (right - left);
        double currentEps = (right - left) / 2;
        while (currentEps > eps) {
            steps.add(new Step(
                    left,
                    right,
                    (right + left) / 2.0,
                    function.apply((right + left) / 2.0)
            ));
            currentEps *= tau;
            if (function.apply(x1) < function.apply(x2)) {
                right = x2;
                x2 = x1;
                x1 = right - tau * (right - left);
            } else {
                left = x1;
                x1 = x2;
                x2 = right - tau * (right - left);
            }
        }
        minArgument = (right + left) / 2.0;
        minValue = function.apply(minArgument);
    }


    @Override
    public Double getMinimumValue() {
        return minValue;
    }

    @Override
    public Double getMinimumArgument() {
        return minArgument;
    }

    @Override
    public List<? extends MethodStep> getSteps() {
        return steps;
    }
}
