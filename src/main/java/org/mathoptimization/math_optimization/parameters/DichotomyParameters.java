package org.mathoptimization.math_optimization.parameters;


import lombok.Data;
import lombok.EqualsAndHashCode;

@EqualsAndHashCode(callSuper = true)
@Data
public class DichotomyParameters extends AbstractParameters {
    private Double firstValue;
    private Double firstArgument;
    private Double secondValue;
    private Double secondArgument;

    public DichotomyParameters(final Double left,
                               final Double right,
                               final Double firstValue,
                               final Double firstArgument,
                               final Double secondValue,
                               final Double secondArgument) {
        super(left, right);
        this.firstValue = firstValue;
        this.firstArgument = firstArgument;
        this.secondValue = secondValue;
        this.secondArgument = secondArgument;
    }
}
