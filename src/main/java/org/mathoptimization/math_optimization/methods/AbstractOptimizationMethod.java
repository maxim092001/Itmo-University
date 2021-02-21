package org.mathoptimization.math_optimization.methods;

import lombok.Data;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

@Data
public abstract class AbstractOptimizationMethod implements OptimizationMethod {
    protected Double left;
    protected Double right;
    protected Function<Double, Double> function;
    protected Double eps;
    protected int stepsCount;
    protected Double minValue;
    protected Double minArgument;
    protected final List<MethodStep> steps = new ArrayList<>();

    public AbstractOptimizationMethod(final Double left,
                                      final Double right,
                                      final Function<Double, Double> function,
                                      final Double eps) {
        this(left, right, function, eps, 0);
    }

    public AbstractOptimizationMethod(final Double left,
                                      final Double right,
                                      final Function<Double, Double> function,
                                      final Double eps,
                                      final int stepsCount) {
        this.left = left;
        this.right = right;
        this.function = function;
        this.eps = eps;
        this.stepsCount = stepsCount;
    }

    @Override
    public Double intervalLength() {
        return right - left;
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
