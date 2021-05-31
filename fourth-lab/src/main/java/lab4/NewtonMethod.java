package lab4;

import java.util.function.Function;

public class NewtonMethod {
    private final Function<Vector, Double> function;
    private final Double eps;
    private final Vector startPoint;
    private final int size;

    public NewtonMethod(final Function<Vector, Double> function, final Double eps, final Vector startPoint) {
        this.function = function;
        this.eps = eps;
        this.startPoint = startPoint;
        this.size = startPoint.size();
    }

    private Vector gradient(final Vector vector) {
        double[] result = new double[size];
        double f0 = function.apply(vector);
        for (int i = 0; i < size; i++) {
            Vector curVector = vector.add(i, eps);
            result[i] = Math.abs(f0 - function.apply(curVector)) / eps;
        }
        return Vector.of(result);
    }

    private double[][] gesseMatrixCalculation(final Vector vector) {
        double[][] result = new double[size][size];
        double f0 = function.apply(vector);
        for (int i = 0; i < size; i++) {
            for (int j = 0; j < size; j++) {
                double fij = function.apply(vector.add(i, eps).add(j, eps));
                double fi = function.apply(vector.add(i, eps));
                double fj = function.apply(vector.add(j, eps));
                result[i][j] = (fij - fi - fj + f0) / (eps * eps);
            }
        }
        return result;
    }

    public Vector optimize() {
        Vector xPrev = startPoint;
        while (true) {
            Vector gradient = gradient(xPrev);
            FullMatrix H = new FullMatrix(gesseMatrixCalculation(xPrev));
            Vector pk = H.gauss(gradient.mul(-1.0), eps).get();
            Vector xK = xPrev.add(pk);
            if (xK.sub(xPrev).norm() < eps) {
                xPrev = xK;
                break;
            }
            xPrev = xK;
        }
        return xPrev;
    }
}
