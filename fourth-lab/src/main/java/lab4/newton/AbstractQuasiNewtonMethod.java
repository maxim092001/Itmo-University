package lab4.newton;

import lab4.matrix.FullMatrix;
import lab4.matrix.Vector;

import java.util.function.Function;

/**
 * Abstract quasi-Newton method.
 */
public abstract class AbstractQuasiNewtonMethod extends OneDirectionNewtonMethod {
    public AbstractQuasiNewtonMethod(final Function<Vector, Double> function, final Double eps, final Vector startPoint) {
        super(function, eps, startPoint);
    }

    /**
     * Minimize.
     *
     * @return point of minimum.
     */
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
            FullMatrix nextG = generateG(prevX, prevW, prevG, prevDX, nextW);
            Vector nextP = nextG.multiply(nextW);
            ak = getAlpha(prevX, nextP);
            Vector nextX = prevX.add(nextP.mul(ak));

            prevG = nextG;
            prevW = nextW;
            prevDX = nextX.sub(prevX);
            prevX = nextX;
        }
        return prevX;
    }

    /**
     * Method special G matrix generation
     *
     * @param prevX  previous point.
     * @param prevW  previous W vector.
     * @param prevG  previous G matrix.
     * @param prevDX previous DX vector.
     * @param nextW  nextW vector.
     * @return new G_k.
     */
    protected abstract FullMatrix generateG(
            final Vector prevX,
            final Vector prevW,
            final FullMatrix prevG,
            final Vector prevDX,
            final Vector nextW
    );
}
