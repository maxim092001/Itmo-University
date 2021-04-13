package lab2.parameters;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.ToString;

/**
 * Parameters for Fibonacci optimization method.
 */
@EqualsAndHashCode(callSuper = true)
@ToString(callSuper = true)
@Data
public class FibonacciParameters extends AbstractParameters {

    /**
     * Second point value. <i>f(x)</i>
     */
    private final Double secondValue;

    /**
     * Second point argument. <i>x</i>
     */
    private final Double secondArgument;

    /**
     * @param left           - Left bound
     * @param right          - Right bound
     * @param value          - Current value. <i>f(x)</i>
     * @param argument       - Current argument. <i>x</i>
     * @param secondValue    - Second point value. <i>f(x)</i>
     * @param secondArgument - Second point argument. <i>x</i>
     */
    public FibonacciParameters(final Double left,
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
