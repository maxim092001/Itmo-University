package lab2.gradient.utils;

import java.util.Arrays;

/**
 * Matrix representation.
 */
public class Matrix {

    /**
     * Given matrix.
     */
    private final double[][] a;

    /**
     * Transposed flag.
     */
    private final boolean transposed;

    /**
     * Constructor for matrix. Transposed {@code false} by default.
     *
     * @param a given matrix.
     */
    public Matrix(final double[][] a) {
        this(a, false);
    }

    /**
     * Constructor for matrix.
     *
     * @param a          given matrix.
     * @param transposed given transposed flag.
     */
    private Matrix(double[][] a, boolean transposed) {
        if (a.length == 0) {
            throw new IllegalArgumentException();
        }

        final int n = a[0].length;
        if (n == 0) {
            throw new IllegalArgumentException();
        }

        for (double[] line : a) {
            if (line.length != n) {
                throw new IllegalArgumentException();
            }
        }

        this.a = a;

        this.transposed = transposed;
    }

    /**
     * Get element from matrix.
     *
     * @param i row index.
     * @param j column index.
     * @return matrix element.
     */
    public double get(int i, int j) {
        return (transposed ? a[j][i] : a[i][j]);
    }

    /**
     * Number of columns.
     *
     * @return number of columns.
     */
    public int verticalLength() {
        return (transposed ? a[0].length : a.length);
    }

    /**
     * Number of rows.
     *
     * @return number of rows.
     */
    public int horizontalLength() {
        return (transposed ? a.length : a[0].length);
    }

    /**
     * Multiply matrix on given scalar number.
     *
     * @param alpha given scalar number.
     * @return multiplied matrix.
     */
    public Matrix mul(final Number alpha) {
        double t = alpha.doubleValue();
        int n = verticalLength();
        int m = horizontalLength();
        double[][] a = new double[n][m];
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < m; j++) {
                a[i][j] = t * get(i, j);
            }
        }
        if (this instanceof Vector) {
            return new Vector(a);
        } else {
            return new Matrix(a);
        }
    }


    /**
     * Add matrix.
     *
     * @param right matrix to be added.
     * @return new matrix.
     */
    public Matrix add(final Matrix right) {
        int n = verticalLength();
        int m = horizontalLength();
        if (n != right.verticalLength() || m != right.horizontalLength()) {
            throw new IllegalArgumentException();
        }

        double[][] a = new double[n][m];
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < m; j++) {
                a[i][j] = get(i, j) + right.get(i, j);
            }
        }
        if (this instanceof Vector) {
            return new Vector(a);
        } else {
            return new Matrix(a);
        }
    }

    /**
     * Multiply matrix on another matrix.
     *
     * @param right given matrix.
     * @return multiplied matrix.
     */
    public Matrix mul(final Matrix right) {
        int n = verticalLength();
        int m = horizontalLength();
        if (right.verticalLength() != m) {
            throw new IllegalArgumentException();
        }
        int k = right.horizontalLength();

        double[][] a = new double[n][k];
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < k; j++) {
                a[i][j] = 0.0;
                for (int l = 0; l < m; l++) {
                    a[i][j] += get(i, l) * right.get(l, j);
                }
            }
        }

        Matrix result = new Matrix(a);
        if (result.horizontalLength() == 1) {
            return new Vector(result.a);
        } else {
            return result;
        }
    }

    /**
     * Transpose matrix.
     * @return transposed matrix.
     */
    public Matrix transpose() {
        return new Matrix(a, !transposed);
    }

    @Override
    public String toString() {
        return "Matrix{" +
                "a=" + Arrays.deepToString(a) +
                ", transposed=" + transposed +
                '}';
    }
}
