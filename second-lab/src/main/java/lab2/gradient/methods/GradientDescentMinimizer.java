package lab2.gradient.methods;

import lab2.gradient.utils.MinPointAndFunction;
import lab2.gradient.utils.QuadraticFunction;
import lab2.gradient.utils.Vector;

/**
 * Gradient descent method.
 */
public class GradientDescentMinimizer extends AbstractGradientMinimizer{
    public MinPointAndFunction newPoint(
            final QuadraticFunction f,
            final Vector point,
            final double eps,
            final double learningRate,
            final double fPoint,
            final Vector gradient
    ) {
        final Vector y = point.sub(gradient.mul(learningRate));
        final double fY = f.apply(y);
        if (fY < fPoint) {
            return minimize(f, learningRate, eps, y);
        } else {
            return newPoint(f, point, eps, learningRate / 2, fPoint, gradient);
        }
    }

    public static void main(String[] args) {
        QuadraticFunction f1 = QuadraticFunction.from2d(1, 2, 3, 4, 5, 6);
        MinPointAndFunction pointAndFunction = new GradientDescentMinimizer().minimize(
                f1,
                120,
                1e-7,
                new Vector(0.0, 0.0)
        );
        System.out.println(pointAndFunction.getPoint().toString());
    }
}
