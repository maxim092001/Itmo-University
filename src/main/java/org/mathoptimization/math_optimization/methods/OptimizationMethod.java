package org.mathoptimization.math_optimization.methods;

import java.util.List;
import java.util.function.Function;

public interface OptimizationMethod {

    void run();

    Double intervalLength();
    Double getMinimumValue();
    Double getMinimumArgument();

    List<? extends MethodStep> getSteps();
}
