package lab2.gradient.methods;

import lab2.gradient.utils.MinimizationResult;
import lab2.gradient.utils.QuadraticFunction;
import lab2.gradient.utils.Vector;

/**
 * Gradient descent method.
 */
public class GradientDescentMinimizer extends AbstractGradientMinimizer{
    public MinimizationResult newPoint(
            final QuadraticFunction f,
            final Vector point,
            final double eps,
            final double learningRate,
            final double fPoint,
            final Vector gradient,
            final long numberOfIterations
    ) {
        final Vector y = point.sub(gradient.mul(learningRate));
        final double fY = f.apply(y);
        if (fY < fPoint) {
            return minimize(f, learningRate, eps, y, numberOfIterations);
        } else {
            return newPoint(f, point, eps, learningRate / 2, fPoint, gradient, numberOfIterations + 1);
        }
    }

    public static void main(String[] args) {
        QuadraticFunction f1 = QuadraticFunction.from2d(1, 2, 3, 4, 5, 6);
        MinimizationResult result = new GradientDescentMinimizer().minimize(
                f1,
                120,
                1e-7,
                new Vector(0.0, 0.0),
                0L
        );
        System.out.println(result);
    }
}
