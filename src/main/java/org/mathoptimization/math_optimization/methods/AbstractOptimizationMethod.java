package org.mathoptimization.math_optimization.methods;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.mathoptimization.math_optimization.parameters.Parameters;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

@Data
@NoArgsConstructor
public abstract class AbstractOptimizationMethod implements OptimizationMethod {

    /**
     * Left bound.
     */
    @JsonProperty("left")
    protected Double left;

    /**
     * Right bound.
     */
    @JsonProperty("right")
    protected Double right;

    /**
     * Function.
     */
    protected Function<Double, Double> function;

    /**
     * Epsilon.
     */
    @JsonProperty("eps")
    protected Double eps;

    /**
     * Custom delta parameter used for Dichotomy method. By default equals 0.
     */
    @JsonProperty("delta")
    protected Double delta;

    /**
     * Custom number of steps used for Fibonacci method. By default equals 0.
     */
    @JsonProperty("steps-count")
    protected int stepsCount;

    /**
     * Minimum value. (f(x))
     */
    protected Double minValue;

    /**
     * Minimum argument. (x)
     */
    protected Double minArgument;


    /**
     * List of parameters used for "logging" our methods steps.
     */
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
