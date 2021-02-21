package org.mathoptimization.math_optimization.methods;

import org.mathoptimization.math_optimization.parameters.Parameters;

import java.util.List;

public interface OptimizationMethod {

    void run();

    Double intervalLength();
    Double getMinimumValue();
    Double getMinimumArgument();

    List<Parameters> getParameters();
}
