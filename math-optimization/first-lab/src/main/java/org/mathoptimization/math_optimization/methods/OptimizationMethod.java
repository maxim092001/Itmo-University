package org.mathoptimization.math_optimization.methods;

import org.mathoptimization.math_optimization.parameters.Parameters;

import java.util.List;

/**
 * Optimization methods interface.
 *
 * @see DichotomyMethod
 * @see FibonacciMethod
 * @see ParabolaMethod
 * @see GoldenRatioMethod
 */
public interface OptimizationMethod {

    /**
     * Each method have to implement its calculation in this method. <br>
     *
     * Results of calculation have to be saved in `minimumArgument` and `minimum value`. <br>
     *
     * For more information see implementations.
     */
    void calculate();

    /**
     * Counts current interval length.
     *
     * @return interval length.
     */
    Double intervalLength();

    /**
     * Current minimum value (f(x)).
     *
     * @return current minimum value.
     */
    Double getMinimumValue();

    /**
     * Current minimum argument (x).
     *
     * @return current minimum value.
     */
    Double getMinimumArgument();

    /**
     * List of parameters used for "logging" our methods steps.
     *
     * @return List of parameters.
     */
    List<Parameters> getParameters();
}
