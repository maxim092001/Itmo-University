package org.mathoptimization.math_optimization.parameters;

public class BrentsParameters extends AbstractParameters {

    /**
     * Parabola coefficients.
     */
    private final Double a0;
    private final Double a1;
    private final Double a2;

    private final Double firstMinimum;
    private final Double firstMinimumValue;

    private final Double secondMinimum;
    private final Double secondMinimumValue;

    private final Double previousSecondMinimum;
    private final Double previousSecondMinimumValue;

    private final Boolean parabolaMethodPassed;

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
                            final Double a2,
                            final Double minArgument,
                            final Double minValue) {
        super(left, right, minValue, minArgument);
        this.a0 = null;
        this.a1 = null;
        this.a2 = a2;
        this.firstMinimum = firstMinimum;
        this.firstMinimumValue = firstMinimumValue;
        this.secondMinimum = secondMinimum;
        this.secondMinimumValue = secondMinimumValue;
        this.previousSecondMinimum = previousSecondMinimum;
        this.previousSecondMinimumValue = previousSecondMinimumValue;
        this.parabolaMethodPassed = true;
    }

    public BrentsParameters(final Double left,
                            final Double right,
                            final Double firstMinimum,
                            final Double firstMinimumValue,
                            final Double secondMinimum,
                            final Double secondMinimumValue,
                            final Double previousSecondMinimum,
                            final Double previousSecondMinimumValue,
                            final Double minArgument,
                            final Double minValue) {
        super(left, right, minValue, minArgument);
        this.a0 = null;
        this.a1 = null;
        this.a2 = null;
        this.firstMinimum = firstMinimum;
        this.firstMinimumValue = firstMinimumValue;
        this.secondMinimum = secondMinimum;
        this.secondMinimumValue = secondMinimumValue;
        this.previousSecondMinimum = previousSecondMinimum;
        this.previousSecondMinimumValue = previousSecondMinimumValue;
        this.parabolaMethodPassed = false;
    }
}
