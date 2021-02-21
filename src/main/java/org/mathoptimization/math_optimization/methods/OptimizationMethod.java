package org.mathoptimization.math_optimization.methods;

import java.util.List;
import java.util.function.Function;

public interface OptimizationMethod<T extends Number> {
    void setFunction(Function<T, T> function);
    void setInterval(T left, T right);

    void setEpsilon(T eps);
    void setStepsCount(int stepsCount);

    void run();

    T getMinimumValue();
    T getMinimumArgument();

    List<MethodStep<T>> getSteps();
}
