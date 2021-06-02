package lab4.newton;

import lab4.matrix.FullMatrix;
import lab4.matrix.Vector;
import lab4.methods.GoldenRatioMethod;

import java.util.function.Function;

public class DavidonFletcherPowellMethod extends AbstractNewtonMethod {
    public DavidonFletcherPowellMethod(final Function<Vector, Double> function, final Double eps, final Vector startPoint) {
        super(function, eps, startPoint);
    }

    @Override
    public double getAlpha(final Vector xk, final Vector pk) {
        var method = new GoldenRatioMethod(-1000d, 1000d, x -> function.apply(xk.add(pk.mul(x))), eps);
        method.calculate();
        return method.getMinArgument();
    }

    @Override
    public Vector minimize() {
        FullMatrix prevG = FullMatrix.identityMatrix(size);
        Vector prevW = gradient(startPoint).mul(-1.0);
        Vector prevP = prevW;
        double ak = getAlpha(startPoint, prevP);
        Vector prevX = startPoint.add(prevP.mul(ak));
        Vector prevDX = prevX.sub(startPoint);

        while (prevDX.norm() > eps) {
            Vector nextW = gradient(prevX).mul(-1.0);
            Vector dw = nextW.sub(prevW);
            Vector v = prevG.multiply(dw);
            FullMatrix nextG = prevG.sub(prevDX.mulByTransposed(prevDX).div(dw.scalarMultiply(prevDX))).sub(v.mulByTransposed(v).div(v.scalarMultiply(dw)));
            Vector nextP = nextG.multiply(nextW);
            ak = getAlpha(prevX, nextP);
            Vector nextX = prevX.add(nextP.mul(ak));

            prevG = nextG;
            prevW = nextW;
            prevX = nextX;
            prevDX = nextX.sub(prevX);
        }
        return prevX;
    }
}
