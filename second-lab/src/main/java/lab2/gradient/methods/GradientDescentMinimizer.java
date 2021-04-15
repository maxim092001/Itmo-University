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

    public MinimizationResult hui(
            final QuadraticFunction f,
            final Vector point,
            final double eps,
            double alpha
    ) {
        int numberOfIterations = 0;
        List<IterationStep> steps = new ArrayList<>();
        Vector gradient = f.gradient(point);
        Vector xk = point;
        Double fX = f.apply(xk);
        while(gradient.rate() > eps) {
            steps.add(new IterationStep(numberOfIterations, xk, fX));
            Vector y = xk.sub(gradient.mul(alpha));
            Double fY = f.apply(y);
            while (fY > fX) {
                alpha /= 2;
                y = xk.sub(gradient.mul(alpha));
                fY = f.apply(y);
            }
            xk = y;
            fX = fY;
            gradient = f.gradient(xk);
            numberOfIterations++;
        }
        steps.add(new IterationStep(numberOfIterations, xk, fX));
        return MinimizationResult.of(xk, fX, numberOfIterations, steps);
    }

    public static void main(String[] args) {
        // lambda1 = 4 - 2 sqrt 2, lambda2 = 4 + 2 sqrt 2
        QuadraticFunction f1 = QuadraticFunction.from2d(64, 126, 64, -10, 30, 13);
        System.out.println(f1);
        Vector startPoint = new Vector(10.0, 15.0);
        List<IterationStep> steps = new ArrayList<>();
        MinimizationResult result = new GradientDescentMinimizer().hui(
                f1,
                startPoint,
                1e-5,
                2 / 256.
        );
        System.out.println(result);

        List<IterationStep> allSteps = result.getSteps();
        for (IterationStep step : allSteps) {
            System.out.println(step);
        }
    }
}
