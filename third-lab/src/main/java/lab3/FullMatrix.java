package lab3;

import java.util.Arrays;

public class FullMatrix {
    private final double[][] matrix;
    private final int n;

    public FullMatrix(final double[][] matrix) {
        this.matrix = matrix;
        this.n = matrix.length;
    }

    public double get(int i, int j) {
        return matrix[i][j];
    }

    public void set(int i, int j, double x) {
        matrix[i][j] = x;
    }

    public void swap(int i, int j, int a, int b) {
        double tmp = matrix[i][j];
        matrix[i][j] = matrix[a][b];
        matrix[a][b] = tmp;
    }

    public Vector gauss(final Vector r, final double eps) {
        final FullMatrix m = copy();
        final Vector v = r.copy();

        final double[] result = new double[n];
        for (int i = 0; i < n; i++) {
            int ind = m.findMainElement(v, i);
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

        return new Vector(result.length, result);
    }

    public int findMainElement(final Vector r, final int k) {
        int ind = k;
        for (int i = k; i < n; i++) {
            if (Math.abs(get(i, k)) > Math.abs(get(ind, k))) {
                ind = i;
            }
        }
        return ind;
    }

    protected FullMatrix copy() {
        double[][] m = new double[n][n];
        for (int i = 0; i < n; i++) {
            m[i] = Arrays.copyOf(matrix[i], n);
        }

        return new FullMatrix(m);
    }
}
