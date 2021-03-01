package org.mathoptimization.math_optimization.methods;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.mathoptimization.math_optimization.parameters.ParabolaParameters;
import org.mathoptimization.math_optimization.parameters.Parameters;

import java.util.function.Function;

@EqualsAndHashCode(callSuper = true)
@Data
@NoArgsConstructor
public class ParabolaMethod extends AbstractOptimizationMethod {

    public ParabolaMethod(final Double left,
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

    private Double findX2(Double x1, Double x3) {
        return Math.random() * (x3 - x1) + x1;
    }

    public static class Parabola {
        private final Double a0;
        private final Double a1;
        private final Double a2;
        private final Double xMin;
        private final Double fxMin;

        Parabola(Function<Double, Double> function, Double x1, Double x2, Double x3, Double f1, Double f2, Double f3) {
            a0 = f1;
            a1 = (f2 - f1) / (x2 - x1);
            a2 = ((f3 - f1) / (x3 - x1) - (f2 - f1) / (x2 - x1)) / (x3 - x2);

            xMin = (x1 + x2 - a1 / a2) / 2.0;
            fxMin = function.apply(xMin);
        }

        public Double getA0() {
            return a0;
        }

        public Double getA1() {
            return a1;
        }

        public Double getA2() {
            return a2;
        }

        public Double getxMin() {
            return xMin;
        }

        public Double getFxMin() {
            return fxMin;
        }
    }


    @Override
    public void calculate() {
        validate();
        Double x1 = left, x3 = right;

        while (x3 - x1 > eps) {
            Double x2 = findX2(x1, x3);
            Double f1 = function.apply(x1);
            Double f2 = function.apply(x2);
            Double f3 = function.apply(x3);


            Parabola parabola = new Parabola(function, x1, x2, x3, f1, f2, f3);
            Double a0 = parabola.getA0();
            Double a1 = parabola.getA1();
            Double a2 = parabola.getA2();

            Double xMin = parabola.getxMin();
            Double fxMin = parabola.getFxMin();

            left = x1;
            right = x3;
            Parameters step = new ParabolaParameters(left, right,
                    f1, x1,
                    a0, a1, a2,
                    xMin, fxMin,
                    f2, f3,
                    x2, x3);

            parameters.add(step);

            if (fxMin < f2) {
                if (xMin < x2) {
                    x3 = x2;
                } else {
                    x1 = x2;
                }
            } else {
                if (xMin < x2) {
                    x1 = xMin;
                } else {
                    x3 = xMin;
                }
            }
        }

        minArgument = (x3 + x1) / 2.0;
        minValue = function.apply(minArgument);

    }
}
