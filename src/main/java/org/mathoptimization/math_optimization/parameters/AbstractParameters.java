package org.mathoptimization.math_optimization.parameters;

public class AbstractParameters implements Parameters {
    protected Double left;
    protected Double right;

    public AbstractParameters() {
    }

    public AbstractParameters(final Double left, final Double right) {
        this.left = left;
        this.right = right;
    }
}
