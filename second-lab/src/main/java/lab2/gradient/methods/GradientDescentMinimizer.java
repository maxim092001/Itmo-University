package lab2.gradient.methods;

import lab2.gradient.utils.MinimizationResult;
import lab2.gradient.utils.QuadraticFunction;
import lab2.gradient.utils.Vector;

import java.util.ArrayList;
import java.util.List;

/**
 * Gradient descent method.
 */
public class GradientDescentMinimizer extends AbstractGradientMinimizer {
    public MinimizationResult newPoint(
            final QuadraticFunction f,
            final Vector point,
            final double eps,
            final double alpha,
            final double fX,
            final Vector gradient,
            final long numberOfIterations,
            final List<IterationStep> steps
    ) {
        final Vector y = point.sub(gradient.mul(alpha));
        final double fY = f.apply(y);
        if (fY < fX) {
            return iteration(f, alpha, eps, y, numberOfIterations, steps);
        } else {
            return newPoint(f, point, eps, alpha / 2, fX, gradient, numberOfIterations, steps);
        }
    }

    public static void main(String[] args) {
        // lambda1 = 4 - 2 sqrt 2, lambda2 = 4 + 2 sqrt 2
        QuadraticFunction f1 = QuadraticFunction.from2d(1, 2, 3, 4, 5, 6);
        System.out.println(f1);
        Vector startPoint = new Vector(0.0, 0.0);
        List<IterationStep> steps = new ArrayList<>();
        MinimizationResult result = new GradientDescentMinimizer().iteration(
                f1,
                0.25,
                1e-5,
                startPoint,
                0L,
                steps
        );
        System.out.println(result);

        List<IterationStep> allSteps = result.getSteps();
        for (IterationStep step : allSteps) {
            System.out.println(step);
        }
    }
}
