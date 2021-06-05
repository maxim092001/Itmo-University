package lab4.newton;

import lab4.matrix.FullMatrix;
import lab4.matrix.Vector;
import lab4.utils.IterationStep;
import lab4.utils.Steps;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

/**
 * Abstract Newton method.
 */
public abstract class AbstractNewtonMethod implements NewtonMethod {

    /**
     * Given function.
     */
    protected final Function<Vector, Double> function;

    /**
     * Given epsilon.
     */
    protected final Double eps;

    /**
     * Start point.
     */
    protected final Vector startPoint;

    /**
     * Start point (vector) size.
     */
    protected final int size;

    /**
     * Iteration steps.
     */
    protected final Steps steps;


    public AbstractNewtonMethod(final Function<Vector, Double> function, final Double eps, final Vector startPoint) {
        this.function = function;
        this.eps = eps;
        this.startPoint = startPoint;
        this.size = startPoint.size();
        this.steps = new Steps(new ArrayList<>(), startPoint);
    }

    /**
     * Gradient calculation for given point.
     *
     * @param vector given point.
     * @return gradient.
     */
    protected Vector gradient(final Vector vector) {
        final double[] result = new double[size];
        final double f0 = function.apply(vector);
        for (int i = 0; i < size; i++) {
            final var curVector = vector.add(i, eps);
            result[i] = (function.apply(curVector) - f0) / eps;
        }
        return Vector.of(result);
    }

    /**
     * Hessian calculation for given point
     *
     * @param vector given point.
     * @return Hessian.
     */
    protected double[][] hesseMatrixCalculation(final Vector vector) {
        final double[][] result = new double[size][size];
        final double f0 = function.apply(vector);
        for (int i = 0; i < size; i++) {
            for (int j = 0; j < size; j++) {
                final double fij = function.apply(vector.add(i, eps).add(j, eps));
                final double fi = function.apply(vector.add(i, eps));
                final double fj = function.apply(vector.add(j, eps));
                result[i][j] = (fij - fi - fj + f0) / (eps * eps);
            }
        }
        return result;
    }


    /**
     * Minimization.
     *
     * @return minimum.
     */
    @Override
    public Vector minimize() {
        Vector xPrev = startPoint;
        while (true) {
            final var gradient = gradient(xPrev);
            final var H = (new FullMatrix(hesseMatrixCalculation(xPrev)));
            final var pk = getDirection(H, gradient, eps, gradient.mul(-1.0));
            final double alpha = getAlpha(xPrev, pk);
            final var xK = xPrev.add(pk.mul(alpha));
            steps.addIteration(alpha, xK, pk, function.apply(xK));
            if (steps.size() > 200000) throw new IllegalArgumentException("Too much iterations");
            if (xK.sub(xPrev).norm() < eps) {
                xPrev = xK;
                break;
            }
            xPrev = xK;
        }
        return xPrev;
    }

    /**
     * Iteration steps.
     *
     * @return iteration steps.
     */
    public Steps getSteps() {
        return steps;
    }
}
