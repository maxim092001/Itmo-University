package org.mathoptimization.math_optimization.parameters;

import lombok.Data;
import lombok.EqualsAndHashCode;

@EqualsAndHashCode(callSuper = true)
@Data
public class GoldenRatioParameters extends AbstractParameters {

    private final Double value;
    private final Double argument;

    public GoldenRatioParameters(final Double left, final Double right, final Double value, final Double argument) {
        super(left, right);
        this.argument = argument;
        this.value = value;
    }

}
