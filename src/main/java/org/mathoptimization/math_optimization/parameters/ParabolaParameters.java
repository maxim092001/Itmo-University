package org.mathoptimization.math_optimization.parameters;


import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.ToString;

@EqualsAndHashCode(callSuper = true)
@ToString(callSuper = true)
@Data
public class ParabolaParameters extends AbstractParameters {

    /**
     * Parabola coefficients.
     */
    private final Double a0;
    private final Double a1;
    private final Double a2;

    /**
     * Min argument. (x)
     */
    private final Double minArgument;

    /**
     * Min value. (f(x))
     */
    private final Double minValue;

    /**
     * Parabola values. (f(x))
     */
    private final Double secondValue;
    private final Double thirdValue;

    /**
     * Parabola arguments. (x)
     */
    private final Double secondArgument;
    private final Double thirdArgument;

    public ParabolaParameters(final Double left,
                              final Double right,
                              final Double value,
                              final Double argument,
                              final Double a0,
                              final Double a1,
                              final Double a2,
                              final Double minArgument,
                              final Double minValue,
                              final Double secondValue,
                              final Double thirdValue,
                              final Double secondArgument,
                              final Double thirdArgument) {
        super(left, right, value, argument);
        this.a0 = a0;
        this.a1 = a1;
        this.a2 = a2;
        this.minArgument = minArgument;
        this.minValue = minValue;
        this.secondValue = secondValue;
        this.thirdValue = thirdValue;
        this.secondArgument = secondArgument;
        this.thirdArgument = thirdArgument;
    }
}
