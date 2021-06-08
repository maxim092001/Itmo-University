package lab3.solvers;

import lab3.Vector;
import lab3.matrix.FullMatrix;

public class GaussSolver implements Solver<FullMatrix> {
    private static final double DEFAULT_EPS = 1e-12;

    public Vector solve(FullMatrix a, Vector b, double eps) {
        final FullMatrix m = a.copy();
        final Vector v = b.copy();
        int n = m.getN();
        assert n == v.size();

        final double[] result = new double[n];
        for (int i = 0; i < n; i++) {
            final int ind = m.findMainElement(i);
            if (Math.abs(m.get(ind, i)) < eps) {
                return null;
            }
            v.swap(i, ind);
            for (int j = i; j < n; j++) {
                m.swap(i, j, ind, j);
            }

            for (int j = i + 1; j < n; j++) {
                double mul = m.get(j, i) / m.get(i, i);
                for (int k = i; k < n; k++) {
                    m.set(j, k, m.get(j, k) - mul * m.get(i, k));
                }
                v.set(j, v.get(j) - mul * v.get(i));
            }
        }

        for (int i = n - 1; i >= 0; i--) {
            result[i] = v.get(i);
            for (int j = i + 1; j < n; j++) {
                result[i] -= m.get(i, j) * result[j];
            }
            result[i] /= m.get(i, i);
        }

        return Vector.of(result);
    }

    @Override
    public Vector solve(FullMatrix a, Vector b) {
        return solve(a, b, DEFAULT_EPS);
    }
}
