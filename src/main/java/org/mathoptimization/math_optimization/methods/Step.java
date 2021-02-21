package org.mathoptimization.math_optimization.methods;

import lombok.Data;

@Data
public class Step implements MethodStep {

    private final Double left;
    private final Double right;
    private final Double value;
    private final Double argument;

    public Step(final Double left, final Double right, final Double value, final Double argument) {
            this.left = left;
            this.right = right;
            this.argument = argument;
            this.value = value;
    }
}
