package lab2.gradient.methods;

import lab2.gradient.utils.MinPointAndFunction;
import lab2.gradient.utils.QuadraticFunction;
import lab2.gradient.utils.Vector;

public abstract class AbstractGradientMinimizer {

    /**
     * Minimizing function.
     *
     * @param f         given function.
     * @param rateValue rate value (alpha or value to calculate alpha).
     * @param eps       epsilon.
     * @param point     given point.
     * @return returns the point where the function reaches its minimum. {@link MinPointAndFunction}
     */
    public MinPointAndFunction minimize(
            final QuadraticFunction f,
            final double rateValue,
            final double eps,
            final Vector point
    ) {
        final double fPoint = f.apply(point);
        final Vector gradient = f.gradient(point);
        if (gradient.rate() < eps) {
            return MinPointAndFunction.of(point, fPoint);
        } else {
            return newPoint(f, point, eps, rateValue, fPoint, gradient);
        }
    }

    /**
     * Finds new point (xk).
     *
     * @param f         given function.
     * @param point     given point.
     * @param eps       epsilon.
     * @param rateValue rate value (alpha or value to calculate alpha).
     * @param fPoint    function result in given point.
     * @param gradient  gradient for given function and point.
     * @return new point.
     */
    public abstract MinPointAndFunction newPoint(
            final QuadraticFunction f,
            final Vector point,
            final double eps,
            final double rateValue,
            final double fPoint,
            final Vector gradient
    );
}
