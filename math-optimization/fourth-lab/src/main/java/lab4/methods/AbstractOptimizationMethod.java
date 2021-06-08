package lab4.methods;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import lab4.parameters.Parameters;
import lombok.Data;
import lombok.NoArgsConstructor;

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
    protected Double left = 0.0;

    /**
     * Right bound.
     */
    @JsonProperty("right")
    protected Double right = 0.0;

    /**
     * Function.
     */
    @JsonIgnore
    protected Function<Double, Double> function = x -> x;

    /**
     * Epsilon.
     */
    @JsonProperty("eps")
    protected Double eps = 1e-9;

    /**
     * Custom delta parameter used for Dichotomy method. By default equals 0.
     */
    @JsonProperty("delta")
    protected Double delta = 0.0;

    /**
     * Custom number of steps used for Fibonacci method. By default equals 0.
     */
    @JsonProperty("steps-count")
    protected int stepsCount = 0;

    /**
     * Minimum value. <i>f(x)</i>
     */
    protected Double minValue;

    /**
     * Minimum argument. <i>x</i>
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

    /**
     * @return Interval length. <i>right - left</i>.
     */
    @Override
    public Double intervalLength() {
        return right - left;
    }

    /**
     * @return current minimum value.
     */
    @Override
    public Double getMinimumValue() {
        return minValue;
    }

    /**
     * @return current minimum argument.
     */
    @Override
    public Double getMinimumArgument() {
        return minArgument;
    }

    /**
     * @return list of method steps.
     */
    @Override
    public List<Parameters> getParameters() {
        return parameters;
    }

    /**
     * Validation method.
     */
    protected abstract void validate();
}
