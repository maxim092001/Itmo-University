package org.mathoptimization.math_optimization.parameters;

import lombok.Data;
import lombok.ToString;

@ToString
@Data
public abstract class AbstractParameters implements Parameters {

    /**
     * Left bound.
     */
    protected Double left;

    /**
     * Right bound.
     */
    protected Double right;

    /**
     * Current value. (f(x))
     */
    protected Double value;

    /**
     * Current argument. (x)
     */
    protected Double argument;

    public AbstractParameters(final Double left, final Double right, final Double value, final Double argument) {
        this.left = left;
        this.right = right;
        this.value = value;
        this.argument = argument;
    }
}
