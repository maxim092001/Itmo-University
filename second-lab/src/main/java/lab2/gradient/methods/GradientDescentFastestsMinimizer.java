package lab2.gradient.methods;

import lab2.gradient.utils.MinPointAndFunction;
import lab2.gradient.utils.QuadraticFunction;
import lab2.gradient.utils.Vector;
import lab2.methods.FibonacciMethod;

public class GradientDescentFastestsMinimizer extends GradientDescentMinimizer {
    public MinPointAndFunction minimize(
            final QuadraticFunction f,
            final double eps,
            final Vector point,
            final double maxEigenvalue
    ) {
        final double fPoint = f.apply(point);
        final Vector gradient = f.gradient(point);
        if (gradient.rate() < eps) {
            return MinPointAndFunction.of(point, fPoint);
        } else {
            return newPoint(f, eps, point, fPoint, gradient, maxEigenvalue);
        }
    }

    private MinPointAndFunction newPoint(
            final QuadraticFunction f,
            final double eps,
            final Vector point,
            final double fPoint,
            final Vector gradient,
            final double maxEigenvalue
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
        final double fY = f.apply(y);
        if (fY < fPoint) {
            return minimize(f, eps, y, maxEigenvalue);
        } else {
            return newPoint(f, eps, point, fPoint, gradient, maxEigenvalue);
        }
    }

    public static void main(String[] args) {
        // l , L --> 2 / (l + L)
        QuadraticFunction f1 = QuadraticFunction.from2d(1, 2, 3, 4, 5, 6);
        MinPointAndFunction pointAndFunction = new GradientDescentFastestsMinimizer().minimize(
                f1,
                1e-7,
                new Vector(0.0, 0.0),
                1
        );
        System.out.println(pointAndFunction.getPoint().toString());
    }

}
