package lab4.newton;

import lab4.matrix.FullMatrix;
import lab4.matrix.Vector;
import lab4.methods.GoldenRatioMethod;

import java.util.function.Function;

/**
 * Davidon-Fletcher-Powell method.
 */
public class DavidonFletcherPowellMethod extends AbstractQuasiNewtonMethod {
    public DavidonFletcherPowellMethod(final Function<Vector, Double> function, final Double eps, final Vector startPoint) {
        super(function, eps, startPoint);
    }

    /**
     * Davidon-Fletcher-Powell method special G matrix generation
     *
     * @param prevX  previous point.
     * @param prevW  previous W vector.
     * @param prevG  previous G matrix.
     * @param prevDX previous DX vector.
     * @param nextW  nextW vector.
     * @return new G_k.
     */
    @Override
    protected FullMatrix generateG(final Vector prevX, final Vector prevW, final FullMatrix prevG, final Vector prevDX, final Vector nextW) {
        Vector dw = nextW.sub(prevW);
        Vector v = prevG.multiply(dw);
        return prevG.sub(prevDX.mulByTransposed(prevDX).div(dw.scalarMultiply(prevDX))).sub(v.mulByTransposed(v).div(v.scalarMultiply(dw)));
    }
}
