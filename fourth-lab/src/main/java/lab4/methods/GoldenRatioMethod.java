package lab4.methods;

import lab4.parameters.GoldenRatioParameters;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.function.Function;

/**
 * Golden-ratio optimization method.
 */
@EqualsAndHashCode(callSuper = true)
@Data
@NoArgsConstructor
public class GoldenRatioMethod extends AbstractOptimizationMethod {
    private static final double sqrt5 = Math.sqrt(5.0);
    private static final double tau = (sqrt5 - 1.0) / 2.0;
    private static final double tauInv = (3.0 - sqrt5) / 2.0;

    public GoldenRatioMethod(final Double left,
                             final Double right,
                             final Function<Double, Double> function,
                             final Double eps) {
        super(left, right, function, eps);
    }

    @Override
    public void validate() {
        if (left == null || right == null || eps == null ||
                eps < 0 || Math.abs(eps) < 1e-9 || left >= right || function == null) {
            throw new RuntimeException();
        }
    }

    @Override
    public void calculate() {
        validate();
        parameters.clear();
        double length = intervalLength();
        double x1 = left + tauInv * length;
        double x2 = left + tau * length;
        double f1 = function.apply(x1);
        double f2 = function.apply(x2);

        double currentEps = length / 2;
        while (currentEps > eps) {
            parameters.add(new GoldenRatioParameters(
                    left,
                    right,
                    f1, x1, f2, x2
            ));
            currentEps *= tau;
            if (f1 < f2) {
                f2 = f1;
                right = x2;
                x2 = x1;
                x1 = right - tau * intervalLength();
                f1 = function.apply(x1);
            } else {
                f1 = f2;
                left = x1;
                x1 = x2;
                x2 = left + tau * intervalLength();
                f2 = function.apply(x2);
            }
        }
        minArgument = (right + left) / 2.0;
        minValue = function.apply(minArgument);
    }
}
