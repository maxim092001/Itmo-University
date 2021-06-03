package lab4.newton;

import lab4.matrix.Vector;
import lab4.methods.GoldenRatioMethod;

import java.util.function.Function;

public class OneDirectionNewtonMethod extends AbstractNewtonMethod {
    public OneDirectionNewtonMethod(final Function<Vector, Double> function, final Double eps, final Vector startPoint) {
        super(function, eps, startPoint);
    }

    @Override
    public double getAlpha(final Vector xk, final Vector pk) {
        var method = new GoldenRatioMethod(-10000d, 10000d, x -> function.apply(xk.add(pk.mul(x))), eps);
        method.calculate();
        return method.getMinArgument();
    }
}
