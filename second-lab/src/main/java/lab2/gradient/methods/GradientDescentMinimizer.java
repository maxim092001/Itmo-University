package lab2.gradient.methods;

import lab2.gradient.utils.MinimizationResult;
import lab2.gradient.utils.QuadraticFunction;
import lab2.gradient.utils.Vector;

import java.util.ArrayList;
import java.util.List;

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
            final long numberOfIterations,
            final List<IterationStep> steps
    ) {
        final Vector y = point.sub(gradient.mul(learningRate));
        final double fY = f.apply(y);
        if (fY < fPoint) {
            return minimize(f, learningRate, eps, y, numberOfIterations, steps);
        } else {
            return newPoint(f, point, eps, learningRate / 2, fPoint, gradient, numberOfIterations, steps);
        }
    }

    public static void main(String[] args) {
        QuadraticFunction f1 = QuadraticFunction.from2d(1, 2, 3, 4, 5, 6);
        Vector startPoint = new Vector(0.0, 0.0);
        List<IterationStep> steps = new ArrayList<>();
        steps.add(new IterationStep(0, startPoint));
        MinimizationResult result = new GradientDescentMinimizer().minimize(
                f1,
                2,
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
