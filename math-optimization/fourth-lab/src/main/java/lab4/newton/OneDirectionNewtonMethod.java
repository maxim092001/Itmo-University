package lab4.newton;

import lab4.matrix.Vector;
import lab4.methods.GoldenRatioMethod;

import java.util.function.Function;

/**
 * One direction Newton method with golden ratio for alpha_k generation.
 */
public class OneDirectionNewtonMethod extends AbstractNewtonMethod {
    public OneDirectionNewtonMethod(final Function<Vector, Double> function, final Double eps, final Vector startPoint) {
        super(function, eps, startPoint);
    }

    /**
     * Generates alpha_k with golden ration method.
     * @param xk point.
     * @param pk direction.
     * @return generated alpha_k.
     */
    @Override
    public double getAlpha(final Vector xk, final Vector pk) {
        var method = new GoldenRatioMethod(-10d, 10d, x -> function.apply(xk.add(pk.mul(x))), eps);
        method.calculate();
        return method.getMinArgument();
    }
}
