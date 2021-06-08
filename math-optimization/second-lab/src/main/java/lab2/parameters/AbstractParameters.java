package lab2.parameters;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

/**
 * Abstraction for method optimization parameters.
 */
@ToString
@Data
@NoArgsConstructor
public abstract class AbstractParameters implements Parameters {

    /**
     * Left bound.
     */
    protected Double left;

    /**
     * Right bound.
     */
    protected Double right;

    /**
     * Current value. <i>f(x)</i>
     */
    protected Double value;

    /**
     * Current argument. <i>x</i>
     */
    protected Double argument;

    /**
     * @param left - Left bound
     * @param right - Right bound
     * @param value - Current value. <i>f(x)</i>
     * @param argument - Current argument. <i>x</i>
     */
    public AbstractParameters(final Double left, final Double right, final Double value, final Double argument) {
        this.left = left;
        this.right = right;
        this.value = value;
        this.argument = argument;
    }
}
