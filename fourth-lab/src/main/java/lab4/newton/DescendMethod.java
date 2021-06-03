package lab4.newton;

import lab4.matrix.FullMatrix;
import lab4.matrix.Vector;

import java.util.function.Function;

/**
 * Descend Newton method.
 */
public class DescendMethod extends OneDirectionNewtonMethod {
    public DescendMethod(final Function<Vector, Double> function, final Double eps, final Vector startPoint) {
        super(function, eps, startPoint);
    }

    /**
     * Generates direction relative to the gradient sign.
     *
     * @param hesseMatrix Hessian.
     * @param gradient    calculated gradient.
     * @param eps         given epsilon.
     * @param defaultP    default vector if no solution found.
     * @return generated direction.
     */
    @Override
    public Vector getDirection(final FullMatrix hesseMatrix, final Vector gradient, final double eps, final Vector defaultP) {
        final var minusGradient = gradient.mul(-1);
        final var p = super.getDirection(hesseMatrix, gradient, eps, defaultP);
        final var scalar = p.scalarMultiply(gradient);
        return scalar > 0 ? minusGradient : p;
    }
}
