package org.mathoptimization.math_optimization.methods;

import java.util.function.Function;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.mathoptimization.math_optimization.methods.ParabolaMethod.Parabola;
import org.mathoptimization.math_optimization.parameters.BrentsParameters;

/**
 * Brent's optimization method.
 */
@EqualsAndHashCode(callSuper = true)
@Data
@NoArgsConstructor
public class BrentsMethod extends AbstractOptimizationMethod {
    private static final double K = (3.0 - Math.sqrt(5.0)) / 2.0;

    public BrentsMethod(final Double left,
                        final Double right,
                        final Function<Double, Double> function,
                        final Double eps) {
        super(left, right, function, eps);
    }

    @Override
    public void validate() {
        if (left == null || right == null || eps == null ||
                equals(eps, 0.0) || eps < 0 || left >= right || function == null) {
            throw new RuntimeException();
        }
    }

    private boolean equals(final double a, final double b) {
        return Math.abs(a - b) < eps;
    }

    private boolean pointsNotEquals(final double a, final double b, final double c) {
        return !equals(a, b) && !equals(a, c) && !equals(b, c);
    }

    @Override
    public void calculate() {
        validate();
        double a = left;
        double c = right;
        double x = a + K * (c - a);
        double w = x;
        double v = x;
        double fx = function.apply(x);
        double fw = fx;
        double fv = fx;
        double d = c - a;
        double e = c - a;

        while (true) {
            double g = e;
            e = d;
            double tol = eps * Math.abs(x)  + eps / 10.0;
            if (Math.abs(x - (a + c) / 2.0) + (c - a) / 2.0 < 2 * tol) {
                break;
            }
            boolean parabolaMethodPassed = false;
            double u = a;

            if (pointsNotEquals(x, w, v) && pointsNotEquals(fx, fw, fv)) {
                Parabola parabola = new Parabola(function, x, w, v, fx, fw, fv);
                u = parabola.getxMin();
                if (a < u && u < c && Math.abs(u - x) < g / 2.0) {
                    u = x - Math.signum(x - (a + c) / 2.0) * tol;
                    parabolaMethodPassed = true;
                    parameters.add(new BrentsParameters(
                            a, c,
                            x, fx, w, fw, v, fv,
                            parabola.getA0(),
                            parabola.getA1(),
                            parabola.getA2()
                    ));
                }
            }

            if (!parabolaMethodPassed) {
                if (x < (a + c) / 2.0) {
                    u = x + K * (c - x);
                    e = c - x;
                } else {
                    u = x - K * (x - a);
                    e = x - a;
                }
                parameters.add(new BrentsParameters(
                        a, c,
                        x, fx, w, fw, v, fv
                ));
            }

            if (Math.abs(u - x) < tol) {
                u = x + Math.signum(u - x) * tol;
            }
            d = Math.abs(u - x);
            double fu = function.apply(u);
            if (fu < fx) {
                if (u > x) {
                    a = x;
                } else {
                    c = x;
                }
                v = w;
                w = x;
                x = u;
                fv = fw;
                fw = fx;
                fx = fu;
            } else {
                if (u > x) {
                    c = u;
                } else {
                    a = u;
                }
                if (fu < fw || equals(w, x)) {
                    v = w;
                    w = u;
                    fv = fw;
                    fw = fu;
                } else if (fu < fv || equals(v, x) || equals(v, w)) {
                    v = u;
                    fv = fu;
                }
            }
        }

        minArgument = (a + c) / 2.0;
        minValue = function.apply(minArgument);
    }
}
