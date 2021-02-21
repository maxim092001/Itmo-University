package org.mathoptimization.math_optimization.parameters;


import lombok.EqualsAndHashCode;
import lombok.ToString;

@EqualsAndHashCode(callSuper = true)
@ToString(callSuper = true)
public class ParabolaParameters extends AbstractParameters {

    private Double a0;
    private Double a1;
    private Double a2;

    private Double xMin;
    private Double fxMin;

    private Double secondValue;
    private Double thirdValue;

    private Double secondArgument;
    private Double thirdArgument;

    public ParabolaParameters(final Double left,
                              final Double right,
                              final Double value,
                              final Double argument,
                              final Double a0,
                              final Double a1,
                              final Double a2,
                              final Double xMin,
                              final Double fxMin,
                              final Double secondValue,
                              final Double thirdValue,
                              final Double secondArgument,
                              final Double thirdArgument) {
        super(left, right, value, argument);
        this.a0 = a0;
        this.a1 = a1;
        this.a2 = a2;
        this.xMin = xMin;
        this.fxMin = fxMin;
        this.secondValue = secondValue;
        this.thirdValue = thirdValue;
        this.secondArgument = secondArgument;
        this.thirdArgument = thirdArgument;
    }
}
