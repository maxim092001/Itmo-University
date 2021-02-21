package org.mathoptimization.math_optimization.methods;

import java.util.List;
import java.util.function.Function;

public interface OptimizationMethod {
    OptimizationMethod setFunction(Function<Double, Double> function);
    OptimizationMethod setInterval(Double left, Double right);

    OptimizationMethod setEpsilon(Double eps);
    OptimizationMethod setStepsCount(int stepsCount);

    void run();

    Double getMinimumValue();
    Double getMinimumArgument();

    List<? extends MethodStep> getSteps();
}
