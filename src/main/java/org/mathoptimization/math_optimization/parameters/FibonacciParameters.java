package org.mathoptimization.math_optimization.parameters;

import lombok.EqualsAndHashCode;
import lombok.ToString;

@EqualsAndHashCode(callSuper = true)
@ToString(callSuper = true)
public class FibonacciParameters extends AbstractParameters {
    public FibonacciParameters(final Double left, final Double right, final Double value, final Double argument) {
        super(left, right, value, argument);
    }
}
