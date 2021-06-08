package org.mathoptimization.math_optimization.parameters;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.ToString;

/**
 * Parameters for Brent's optimization method.
 */
@EqualsAndHashCode(callSuper = true)
@ToString(callSuper = true)
@Data
public class BrentsParameters extends AbstractParameters {

    /**
     * Parabola coefficient.
     */
    private final Double a0;

    /**
     * Parabola coefficient.
     */
    private final Double a1;

    /**
     * Parabola coefficient.
     */
    private final Double a2;

    /**
     * Optimization argument. <i>xs</i>
     */
    private final Double firstMinimum;

    /**
     * Optimization argument. <i>xs</i>
     */
    private final Double secondMinimum;

    /**
     * Optimization argument. <i>xs</i>
     */
    private final Double previousSecondMinimum;


    /**
     * Optimization value. <i>f(xs)</i>
     */
    private final Double firstMinimumValue;

    /**
     * Optimization value. <i>f(xs)</i>
     */
    private final Double secondMinimumValue;

    /**
     * Optimization value. <i>f(xs)</i>
     */
    private final Double previousSecondMinimumValue;

    /**
     * @param left                       - Left bound
     * @param right                      - Right bound
     * @param firstMinimum               - <i>xs</i>
     * @param firstMinimumValue          - Optimization value. <i>f(xs)</i>
     * @param secondMinimum              - Optimization argument. <i>xs</i>
     * @param secondMinimumValue         - Optimization value. <i>f(xs)</i>
     * @param previousSecondMinimum      - Optimization argument. <i>xs</i>
     * @param previousSecondMinimumValue - Optimization value. <i>f(xs)</i>
     * @param a0                         - Parabola coefficient.
     * @param a1                         - Parabola coefficient.
     * @param a2                         - Parabola coefficient.
     */
    public BrentsParameters(final Double left,
                            final Double right,
                            final Double firstMinimum,
                            final Double firstMinimumValue,
                            final Double secondMinimum,
                            final Double secondMinimumValue,
                            final Double previousSecondMinimum,
                            final Double previousSecondMinimumValue,
                            final Double a0,
                            final Double a1,
                            final Double a2) {
        super(left, right, null, null);
        this.a0 = a0;
        this.a1 = a1;
        this.a2 = a2;
        this.firstMinimum = firstMinimum;
        this.firstMinimumValue = firstMinimumValue;
        this.secondMinimum = secondMinimum;
        this.secondMinimumValue = secondMinimumValue;
        this.previousSecondMinimum = previousSecondMinimum;
        this.previousSecondMinimumValue = previousSecondMinimumValue;
    }

    /**
     * @param left                       - Left bound
     * @param right                      - Right bound
     * @param firstMinimum               - <i>xs</i>
     * @param firstMinimumValue          - Optimization value. <i>f(xs)</i>
     * @param secondMinimum              - Optimization argument. <i>xs</i>
     * @param secondMinimumValue         - Optimization value. <i>f(xs)</i>
     * @param previousSecondMinimum      - Optimization argument. <i>xs</i>
     * @param previousSecondMinimumValue - Optimization value. <i>f(xs)</i>
     */
    public BrentsParameters(final Double left,
                            final Double right,
                            final Double firstMinimum,
                            final Double firstMinimumValue,
                            final Double secondMinimum,
                            final Double secondMinimumValue,
                            final Double previousSecondMinimum,
                            final Double previousSecondMinimumValue) {
        super(left, right, null, null);
        this.a0 = null;
        this.a1 = null;
        this.a2 = null;
        this.firstMinimum = firstMinimum;
        this.firstMinimumValue = firstMinimumValue;
        this.secondMinimum = secondMinimum;
        this.secondMinimumValue = secondMinimumValue;
        this.previousSecondMinimum = previousSecondMinimum;
        this.previousSecondMinimumValue = previousSecondMinimumValue;
    }
}
