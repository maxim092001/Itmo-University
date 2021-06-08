package lab3.matrix;

import lab3.Vector;

import java.util.Arrays;
import java.util.Optional;

/**
 * Full matrix implementation.
 */
public class FullMatrix implements Matrix<FullMatrix> {

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

    @Override
    public int getN() {
        return n;
    }

    @Override
    public Vector mul(Vector b) {
        throw new UnsupportedOperationException();
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
    public void swap(final int i, final int j, final int a, final int b) {
        double tmp = matrix[i][j];
        matrix[i][j] = matrix[a][b];
        matrix[a][b] = tmp;
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
    public FullMatrix copy() {
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
}
