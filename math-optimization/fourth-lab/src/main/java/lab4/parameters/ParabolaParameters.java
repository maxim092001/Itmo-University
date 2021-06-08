package lab4.parameters;


import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.ToString;


/**
 * Parameters for Parabola optimization method.
 */
@EqualsAndHashCode(callSuper = true)
@ToString(callSuper = true)
@Data
public class ParabolaParameters extends AbstractParameters {

    /**
     * Parabola coefficient.
     */
    private final Double a0, a1, a2;

    /**
     * Min argument. <i>x</i>
     */
    private final Double minArgument;

    /**
     * Min value.  <i>f(x)</i>
     */
    private final Double minValue;

    /**
     * Parabola value. <i>f(x)</i>
     */
    private final Double secondValue, thirdValue;

    /**
     * Parabola argument. <i>x</i>
     */
    private final Double secondArgument, thirdArgument;

    /**
     * @param left           - Left bound
     * @param right          - Right bound
     * @param value          - Current value. <i>f(x)</i>
     * @param argument       - Current argument. <i>x</i>
     * @param a0             - Parabola coefficient.
     * @param a1             - Parabola coefficient.
     * @param a2             - Parabola coefficient.
     * @param minArgument    -  Min argument. <i>x</i>
     * @param minValue       - Min value.  <i>f(x)</i>
     * @param secondValue    - Parabola value. <i>f(x)</i>
     * @param thirdValue     - Parabola value. <i>f(x)</i>
     * @param secondArgument - Parabola argument. <i>x</i>
     * @param thirdArgument  - Parabola argument. <i>x</i>
     */
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
