package lab2.gradient;

/**
 * Gradient descent method.
 */
public class GradientDescentMinimizer implements GradientMinimizer {

    /**
     * Minimizing function.
     *
     * @param f            given function.
     * @param learningRate given learning rate (alpha).
     * @param eps          epsilon.
     * @param point        given point.
     * @return returns the point where the function reaches its minimum. {@link MinPointAndFunction}
     */
    public MinPointAndFunction minimize(
            final QuadraticFunction f,
            final double learningRate,
            final double eps,
            final Vector point
    ) {
        final double fPoint = f.apply(point);
        final Vector gradient = f.gradient(point);
        if (gradient.rate() < eps) {
            return MinPointAndFunction.of(point, fPoint);
        } else {
            return newPoint(f, learningRate, eps, point, fPoint, gradient);
        }
    }

    /**
     * Finds new point (xk).
     *
     * @param f            given function.
     * @param learningRate learning rate (alpha.
     * @param eps          epsilon.
     * @param point        given point.
     * @param fPoint       function result in given point.
     * @param gradient     gradient for given function and point.
     * @return new point.
     */
    private MinPointAndFunction newPoint(
            final QuadraticFunction f,
            final double learningRate,
            final double eps,
            final Vector point,
            final double fPoint,
            final Vector gradient
    ) {
        final Vector y = point.sub(gradient.mul(learningRate));
        final double fY = f.apply(y);
        if (fY < fPoint) {
            return minimize(f, learningRate, eps, y);
        } else {
            return newPoint(f, learningRate / 2, eps, point, fPoint, gradient);
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
