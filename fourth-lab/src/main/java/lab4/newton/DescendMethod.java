package lab4.newton;

import lab4.matrix.FullMatrix;
import lab4.matrix.Vector;
import lab4.methods.GoldenRatioMethod;

import java.util.function.Function;

public class DescendMethod extends AbstractNewtonMethod {
    public DescendMethod(final Function<Vector, Double> function, final Double eps, final Vector startPoint) {
        super(function, eps, startPoint);
    }

    @Override
    public Vector getDirection(final FullMatrix hesseMatrix, final Vector gradient, final double eps) {
        final var minusGradient = gradient.mul(-1);
        final var p = super.getDirection(hesseMatrix, gradient, eps);
        final var scalar = p.scalarMultiply(gradient);
        return scalar > 0 ? minusGradient : p;
    }

    // TODO remove copypast
    @Override
    public double getAlpha(final Vector xk, final Vector pk) {
        var method = new GoldenRatioMethod(-1000d, 1000d, x -> function.apply(xk.add(pk.mul(x))), eps);
        method.calculate();
        return method.getMinArgument();
    }
}
