package org.mathoptimization.math_optimization.methods;

public interface MethodStep {
    Double getLeft();
    Double getRight();
    Double getValue();
    Double getArgument();
}
