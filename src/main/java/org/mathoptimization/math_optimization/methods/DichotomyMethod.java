package org.mathoptimization.math_optimization.methods;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.mathoptimization.math_optimization.parameters.DichotomyParameters;

import java.util.function.Function;

@EqualsAndHashCode(callSuper = true)
@Data
@NoArgsConstructor
public class DichotomyMethod extends AbstractOptimizationMethod {


    public DichotomyMethod(final Double left,
                           final Double right,
                           final Function<Double, Double> function,
                           final Double eps,
                           final Double delta) {
        super(left, right, function, eps, delta);
    }

    @Override
    public void calculate() {
        parameters.clear();
        double b = right, a = left;

        while ((b - a) / 2.0 > eps) {
            double x1 = (b + a - delta) / 2.0;
            double x2 = (b + a + delta) / 2.0;

            double f1 = function.apply(x1);
            double f2 = function.apply(x2);

            parameters.add(new DichotomyParameters(a, b, f1, x1, f2, x2));

            if (f1 <= f2) {
                b = x2;
            } else {
                a = x1;
            }
        }

        minArgument = (a + b) / 2.0;
        minValue = function.apply(minArgument);
    }
}
