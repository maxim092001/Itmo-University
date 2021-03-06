package lab4.matrix;

import java.util.Arrays;
import java.util.Optional;

/**
 * Full matrix implementation.
 */
public class FullMatrix implements Matrix {

    /**
     * Matrix.
     */
    private final double[][] matrix;

    /**
     * Dimension.
     */
    private final int n;

    public FullMatrix(final double[][] matrix) {
        this.matrix = matrix;
        this.n = matrix.length;
    }

    /**
     * Gets element from profile matrix.
     *
     * @param i row index.
     * @param j column index.
     * @return elements value.
     */
    public double get(final int i, final int j) {
        return matrix[i][j];
    }

    /**
     * Sets value to an element in profile matrix.
     *
     * @param i     row index.
     * @param j     column index.
     * @param value given value to set.
     */
    public void set(final int i, final int j, double value) {
        matrix[i][j] = value;
    }

    /**
     * Swaps elements in matrix
     *
     * @param i first row index.
     * @param j first column index.
     * @param a second row index.
     * @param b second column index.
     */
    private void swap(final int i, final int j, final int a, final int b) {
        double tmp = matrix[i][j];
        matrix[i][j] = matrix[a][b];
        matrix[a][b] = tmp;
    }

    /**
     * Finds solution for full matrix with gauss method.
     *
     * @param r given vector.
     * @param eps epsilon.
     * @return {@link Optional}. Empty if no solution found or solution.
     */
    public Optional<Vector> gauss(final Vector r, final double eps) {
        final FullMatrix m = copy();
        final Vector v = r.copy();

        final double[] result = new double[n];
        for (int i = 0; i < n; i++) {
            final int ind = m.findMainElement(i);
            if (Math.abs(m.get(ind, i)) < eps) {
                return Optional.empty();
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

        return Optional.of(Vector.of(result));
    }

    /**
     * Finds main element in current matrix with given k.
     *
     * @param k given k.
     * @return index of main element.
     */
    public int findMainElement(final int k) {
        int ind = k;
        for (int i = k; i < n; i++) {
            if (Math.abs(get(i, k)) > Math.abs(get(ind, k))) {
                ind = i;
            }
        }
        return ind;
    }

    /**
     * Copies current matrix.
     *
     * @return copy of current matrix.
     */
    private FullMatrix copy() {
        double[][] m = new double[n][n];
        for (int i = 0; i < n; i++) {
            m[i] = Arrays.copyOf(matrix[i], n);
        }

        return new FullMatrix(m);
    }

    /**
     * Creating representation of current matrix into profile.
     *
     * @return profile matrix.
     */
    public ProfileMatrix toProfileMatrix() {
        return ProfileMatrix.of(matrix);
    }

    /**
     * Identity matrix of given size.
     * @param size given size.
     * @return identity matrix.
     */
    public static FullMatrix identityMatrix(final int size) {
        double[][] m = new double[size][size];
        for (int i = 0; i < size; i++) {
            m[i][i] = 1.0;
        }
        return new FullMatrix(m);
    }

    /**
     * Multiply matrix on given vector.
     * @param vector given vector.
     * @return multiplication result.
     */
    public Vector multiply(final Vector vector) {
        final double[] result = new double[n];
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                result[i] += get(i, j) * vector.get(j);
            }
        }
        return Vector.of(result);
    }

    /**
     * Scalar division on given value.
     * @param value given value.
     * @return divided matrix.
     */
    public FullMatrix div(final double value) {
        final double[][] result = new double[n][n];
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                result[i][j] = get(i, j) / value;
            }
        }
        return new FullMatrix(result);
    }

    /**
     * Subtract two matrices.
     * @param other given matrix.
     * @return subtracted matrix.
     */
    public FullMatrix sub(final FullMatrix other) {
        final double[][] result = new double[n][n];
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                result[i][j] = get(i, j) - other.get(i, j);
            }
        }
        return new FullMatrix(result);
    }
}
