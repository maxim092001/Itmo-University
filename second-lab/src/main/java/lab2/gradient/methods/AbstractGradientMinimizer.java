package lab2.gradient.methods;

import lab2.gradient.utils.MinimizationResult;
import lab2.gradient.utils.QuadraticFunction;
import lab2.gradient.utils.Vector;

import java.util.List;

/**
 * Abstract gradient minimizer.
 */
public abstract class AbstractGradientMinimizer {

    /**
     * Minimizing function.
     *
     * @param f     given function.
     * @param alpha rate value (alpha or value to calculate alpha).
     * @param eps   epsilon.
     * @param x     given point.
     * @return returns the point where the function reaches its minimum. {@link MinimizationResult}
     */
    public MinimizationResult iteration(
            final QuadraticFunction f,
            final double alpha,
            final double eps,
            final Vector x,
            final long numberOfIterations,
            final List<IterationStep> steps
    ) {
        steps.add(new IterationStep(numberOfIterations, x));

        final double fPoint = f.apply(x);
        final Vector gradient = f.gradient(x);

        if (gradient.rate() < eps || numberOfIterations >= 1000) {
            return MinimizationResult.of(x, fPoint, numberOfIterations, steps);
        } else {
            return this.newPoint(f, x, eps, alpha, fPoint, gradient, numberOfIterations + 1, steps);
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
    public abstract MinimizationResult newPoint(
            final QuadraticFunction f,
            final Vector point,
            final double eps,
            final double rateValue,
            final double fPoint,
            final Vector gradient,
            final long numberOfIterations,
            final List<IterationStep> steps
    );
}
