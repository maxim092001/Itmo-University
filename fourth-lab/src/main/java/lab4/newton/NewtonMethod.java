package lab4.newton;

import lab4.matrix.FullMatrix;
import lab4.matrix.Vector;

import java.util.function.Function;

public interface NewtonMethod {

    Vector minimize();
    default double getAlpha(final Vector xk, final Vector pk) {
        return 1.0;
    }

    default Vector getDirection(final FullMatrix hesseMatrix, final Vector gradient, final double eps, final Vector defaultP) {
        return hesseMatrix.gauss(gradient.mul(-1), eps).
                orElse(defaultP);
    }
}
