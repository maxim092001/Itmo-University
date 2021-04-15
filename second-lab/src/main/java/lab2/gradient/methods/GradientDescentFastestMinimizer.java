package lab2.gradient.methods;

import lab2.gradient.utils.MinimizationResult;
import lab2.gradient.utils.QuadraticFunction;
import lab2.gradient.utils.Vector;
import lab2.methods.FibonacciMethod;

import java.util.ArrayList;
import java.util.List;

/**
 * Gradient fastest descent minimizer.
 */
public class GradientDescentFastestMinimizer extends AbstractGradientMinimizer {
    public MinimizationResult newPoint(
            final QuadraticFunction f,
            final Vector point,
            final double eps,
            final double maxEigenvalue,
            final double fPoint,
            final Vector gradient,
            final long numberOfIterations,
            final List<IterationStep> steps
    ) {
        final var method = new FibonacciMethod(
                0.0,
                maxEigenvalue,
                alpha -> f.apply(point.sub((f.gradient(point)).mul(alpha))),
                eps
        );
        method.calculate();
        final double learningRate = method.getMinimumArgument();
        final Vector y = point.sub(gradient.mul(learningRate));
        return iteration(f, maxEigenvalue, eps, y, numberOfIterations, steps);
    }

    public static void main(String[] args) {
        // l , L --> 2 / (l + L)
        QuadraticFunction f1 = QuadraticFunction.from2d(1, 2, 3, 4, 5, 6);
        Vector startPoint = new Vector(0.0, 0.0);
        List<IterationStep> steps = new ArrayList<>();
        steps.add(new IterationStep(0, startPoint));
        MinimizationResult result = new GradientDescentFastestMinimizer().iteration(
                f1,
                1,
                1e-5,
                startPoint,
                0L,steps
        );
        System.out.println(result);

        List<IterationStep> allSteps = result.getSteps();
        for (IterationStep step : allSteps) {
            System.out.println(step);
        }
    }

}
