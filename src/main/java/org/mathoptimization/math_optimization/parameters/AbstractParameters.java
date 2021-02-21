package org.mathoptimization.math_optimization.parameters;

import lombok.Data;
import lombok.ToString;

@ToString
@Data
public class AbstractParameters implements Parameters {
    protected Double left;
    protected Double right;
    protected Double value;
    protected Double argument;

    public AbstractParameters() {
    }

    public AbstractParameters(final Double left, final Double right, final Double value, final Double argument) {
        this.left = left;
        this.right = right;
        this.value = value;
        this.argument = argument;
    }
}
