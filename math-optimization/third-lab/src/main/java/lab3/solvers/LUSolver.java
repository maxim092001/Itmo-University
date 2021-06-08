package lab3.solvers;

import lab3.Vector;
import lab3.matrix.LUMatrix;

public class LUSolver implements Solver<LUMatrix> {
    @Override
    public Vector solve(LUMatrix a, Vector b) {
        int n = a.getN();

        double[] y = new double[n];
        double sum;
        for (int i = 0; i < n; i++) {
            sum = 0;
            for (int k = 0; k < i; k++) {
                sum += a.get(i, k) * y[k];
            }
            y[i] = b.get(i) - sum;
        }

        double[] x = new double[n];
        for (int i = n - 1; i >= 0; i--) {
            sum = 0;
            for (int k = i + 1; k < n; k++) {
                sum += a.get(i, k) * x[k];
            }
            x[i] = (y[i] - sum) / (a.get(i, i));
        }
        return Vector.of(x);
    }
}
