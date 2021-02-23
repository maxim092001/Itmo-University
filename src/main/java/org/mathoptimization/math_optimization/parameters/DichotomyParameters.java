package org.mathoptimization.math_optimization.parameters;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.ToString;

@EqualsAndHashCode(callSuper = true)
@ToString(callSuper = true)
@Data
public class DichotomyParameters extends AbstractParameters {

    /**
     * Second point value. f(x)
     */
    private final Double secondValue;

    /**
     * Second point argument. (x)
     */
    private final Double secondArgument;

    public DichotomyParameters(final Double left,
                               final Double right,
                               final Double value,
                               final Double argument,
                               final Double secondValue,
                               final Double secondArgument) {
        super(left, right, value, argument);
        this.secondValue = secondValue;
        this.secondArgument = secondArgument;
    }
}
