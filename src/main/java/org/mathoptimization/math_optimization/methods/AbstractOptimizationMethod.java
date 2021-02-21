package org.mathoptimization.math_optimization.methods;

import lombok.Data;
import org.mathoptimization.math_optimization.parameters.Parameters;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

@Data
public abstract class AbstractOptimizationMethod implements OptimizationMethod {
    protected Double left;
    protected Double right;
    protected Function<Double, Double> function;
    protected Double eps;
    protected Double delta;
    protected int stepsCount;
    protected Double minValue;
    protected Double minArgument;
    protected final List<Parameters> parameters = new ArrayList<>();

    public AbstractOptimizationMethod(final Double left,
                                      final Double right,
                                      final Function<Double, Double> function,
                                      final Double eps,
                                      final Double delta) {
        this(left, right, function, eps, 0, delta);
    }

    public AbstractOptimizationMethod(final Double left,
                                      final Double right,
                                      final Function<Double, Double> function,
                                      final Double eps) {
        this(left, right, function, eps, 0, 0.0);
    }

    public AbstractOptimizationMethod(final Double left,
                                      final Double right,
                                      final Function<Double, Double> function,
                                      final Double eps,
                                      final int stepsCount,
                                      final Double delta) {
        this.left = left;
        this.right = right;
        this.function = function;
        this.eps = eps;
        this.stepsCount = stepsCount;
        this.delta = delta;
    }

    public AbstractOptimizationMethod(final Double left,
                                      final Double right,
                                      final Function<Double, Double> function,
                                      final Double eps,
                                      final int stepsCount) {
        this(left, right,function, eps, stepsCount, 0.0);
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
    public List<Parameters> getParameters() {
        return parameters;
    }
}
