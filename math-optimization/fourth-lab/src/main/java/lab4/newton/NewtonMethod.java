package lab4.newton;

import lab4.matrix.FullMatrix;
import lab4.matrix.Vector;

/**
 * Interface for newton methods.
 */
public interface NewtonMethod {

    /**
     * Find minimum.
     *
     * @return minimum.
     */
    Vector minimize();

    /**
     * Calculate alpha for current method. Default is 1.0.
     *
     * @param xk point.
     * @param pk direction.
     * @return alpha.
     */
    default double getAlpha(final Vector xk, final Vector pk) {
        return 1.0;
    }

    /**
     * Calculate direction for current method. Default is gauss calculation.
     *
     * @param hesseMatrix Hessian.
     * @param gradient    calculated gradient.
     * @param eps         given epsilon.
     * @param defaultP    default vector if no solution found.
     * @return calculated direction.
     */
    default Vector getDirection(final FullMatrix hesseMatrix, final Vector gradient, final double eps, final Vector defaultP) {
        return hesseMatrix.gauss(gradient.mul(-1), eps).
                orElse(defaultP);
    }
}
