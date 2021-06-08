package lab4.newton;

import lab4.matrix.Vector;

import java.util.function.Function;

/**
 * Classic Newton method.
 */
public class ClassicNewtonMethod extends AbstractNewtonMethod {

    public ClassicNewtonMethod(final Function<Vector, Double> function, final Double eps, final Vector startPoint) {
        super(function, eps, startPoint);
    }
}
