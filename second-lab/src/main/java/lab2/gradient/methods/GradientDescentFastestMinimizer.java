package lab2.gradient.methods;

import lab2.gradient.utils.DiagMatrix;
import lab2.gradient.utils.MinimizationResult;
import lab2.gradient.utils.QuadraticFunction;
import lab2.gradient.utils.Vector;
import lab2.methods.BrentsMethod;
import lab2.methods.DichotomyMethod;
import lab2.methods.FibonacciMethod;
import lab2.methods.GoldenRatioMethod;

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
        final var method = new GoldenRatioMethod(
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

    public MinimizationResult hui(
            final QuadraticFunction f,
            final Vector point,
            final double eps,
            final double maxEigenvalue
    ) {
        int numberOfIterations = 0;
        List<IterationStep> steps = new ArrayList<>();
        Vector gradient = f.gradient(point);
        Vector xk = point;
        Double fX = f.apply(xk);
        while(gradient.rate() > eps) {
            steps.add(new IterationStep(numberOfIterations, xk, fX));
            final var method = new GoldenRatioMethod(
                    0.0,
                    maxEigenvalue,
                    alpha -> f.apply(point.sub((f.gradient(point)).mul(alpha))),
                    eps
            );
            method.calculate();
            final double alpha = method.getMinimumArgument();
            Vector y = xk.sub(gradient.mul(alpha));
            Double fY = f.apply(y);
            xk = y;
            fX = fY;
            gradient = f.gradient(xk);
            numberOfIterations++;
        }
        steps.add(new IterationStep(numberOfIterations, xk, fX));
        return MinimizationResult.of(xk, fX, numberOfIterations, steps);
    }

    public static void main(String[] args) {
        // l , L --> 2 / (l + L)
        QuadraticFunction f1 = QuadraticFunction.from2d(64, 126, 64, -10, 30, 13);
        Vector startPoint = new Vector(10.0, 15.0);
        List<IterationStep> steps = new ArrayList<>();
        MinimizationResult result = new GradientDescentFastestMinimizer().iteration(
                f1,
                2 / 256.0,
                1e-5,
                startPoint,
                0L,
                steps
        );
        System.out.println(result);

        List<IterationStep> allSteps = result.getSteps();
        System.out.println("X");
        for (IterationStep step : allSteps) {
            System.out.print(step.getVector().get(0) + ", ");
        }

//        for (int i = 0; i < 100; i++) {
//            System.out.print(allSteps.get(i).getVector().get(0) + ", ");
//        }

        System.out.println();
        System.out.println("Y");
        for (IterationStep step : allSteps) {
            System.out.print(step.getVector().get(1) + ", ");
        }

//        for (int i = 0; i < 100; i++) {
//            System.out.print(allSteps.get(i).getVector().get(1) + ", ");
//        }

        System.out.println();

        for (int n = 10; n <= 10000; n *= 10) {
            System.out.println("Dimensions = " + n + ":");
            for (int k = 1; k < 2000; k += 100) {
                QuadraticFunction g = new QuadraticFunction(new DiagMatrix(n, k), Vector.randomVector(n), 0);
                Vector sp = Vector.randomVector(n);
                MinimizationResult res = new GradientDescentFastestMinimizer().hui(
                        g,
                        sp,
                        1e-3,
                        (double)(k + 1)
                );
                System.out.print(res.getNumberOfIterations() + ", ");
            }
            System.out.println();
        }
    }

}
