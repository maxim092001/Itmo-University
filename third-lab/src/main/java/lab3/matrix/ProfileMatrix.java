package lab3.matrix;

import lab3.Vector;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Representation of profile matrix.
 */
public class ProfileMatrix implements Matrix {
    private static final double EPS = 1e-7;

    /**
     * Matrix dimension.
     */
    final int n;

    /**
     * Matrix elements starting from first non-zero above main diagonal.
     */
    final double[] au;

    /**
     * Elements located on right part of equatation.
     */
    final double[] r;

    /**
     * Matrix elements starting from first non-zero below main diagonal.
     */
    final double[] al;


    /**
     * "Matrix profile".
     */
    final int[] ia;

    /**
     * Main diagonal values.
     */
    final double[] di;

    private ProfileMatrix(
            final int n,
            final double[] au,
            final double[] al,
            final int[] ia,
            final double[] di,
            final double[] r
    ) {
        this.n = n;
        this.au = au;
        this.al = al;
        this.ia = ia;
        this.di = di;
        this.r = r;
    }

    private ProfileMatrix(final double[][] matrix) {
        di = new double[matrix.length];
        List<Double> au = new ArrayList<>();
        List<Double> al = new ArrayList<>();
        List<Integer> ia = new ArrayList<>(List.of(1));

        for (int i = 0; i < matrix.length; i++) {
            di[i] = matrix[i][i];
            int firstNonZero = -1;
            for (int j = 0; j < i; j++) {
                if (firstNonZero == -1 && (Math.abs(matrix[i][j]) > EPS || Math.abs(matrix[j][i]) > EPS)) {
                    firstNonZero = j;
                    ia.add(ia.get(ia.size() - 1) + (i - firstNonZero));
                }
                if (firstNonZero != -1) {
                    au.add(matrix[j][i]);
                    al.add(matrix[i][j]);
                }
            }
            if (firstNonZero == -1) {
                ia.add(ia.get(ia.size() - 1));
            }
        }

        this.n = matrix.length;
        this.au = au.stream().mapToDouble(i -> i).toArray();
        this.al = al.stream().mapToDouble(i -> i).toArray();
        this.ia = ia.stream().mapToInt(i -> i).toArray();
        this.r = null;

    }

    public static ProfileMatrix of(
            final int n,
            final double[] au,
            final double[] al,
            final int[] ia,
            final double[] di,
            final double[] r
    ) {
        return new ProfileMatrix(n, au, al, ia, di, r);
    }

    public static ProfileMatrix of(final double[][] matrix) {
        return new ProfileMatrix(matrix);
    }


    /**
     * Gets element from profile matrix.
     *
     * @param i row index.
     * @param j column index.
     * @return elements value.
     */
    public double get(int i, int j) {
        if (i == j) {
            return di[i];
        } else if (i < j) {
            int profilelNum = ia[j + 1] - ia[j];
            int firstInProfile = j - profilelNum;
            return i < firstInProfile ? 0 : au[ia[j] + i - firstInProfile - 1];
        } else {
            int profilelNum = ia[i + 1] - ia[i];
            int firstInProfile = i - profilelNum;
            return j < firstInProfile ? 0 : al[ia[i] + j - firstInProfile - 1];
        }
    }

    /**
     * Sets value to an element in profile matrix.
     *
     * @param i     row index.
     * @param j     column index.
     * @param value given value to set.
     */
    public void set(int i, int j, double value) {
        if (i == j) {
            di[i] = value;
        } else if (i < j) {
            int profilelNum = ia[j + 1] - ia[j];
            int firstInProfile = j - profilelNum;
            if (i >= firstInProfile) {
                au[ia[j] + i - firstInProfile - 1] = value;
            }
        } else {
            int profilelNum = ia[i + 1] - ia[i];
            int firstInProfile = i - profilelNum;
            if (j >= firstInProfile) {
                al[ia[i] + j - firstInProfile - 1] = value;
            }
        }
    }

    /**
     * Function to calculate LU decomposition for this profile matrix.
     */
    public void computeLUDecomposition() {
        double sum;
        for (int i = 0; i < n; i++) {
            for (int j = i; j < n; j++) {
                sum = 0;
                for (int k = 0; k < i; k++) {
                    sum += get(i, k) * get(k, j);
                }
                set(i, j, get(i, j) - sum);
            }
            for (int j = i + 1; j < n; j++) {
                sum = 0;
                for (int k = 0; k < i; k++) {
                    sum += get(j, k) * get(k, i);
                }
                set(j, i, (1.0 / get(i, i)) * (get(j, i) - sum));
            }
        }
    }

    /**
     * Solves LU decomposition.
     * @param r given vector.
     * @return solution.
     */
    public Vector solve(final Vector r) {
        double[] y = new double[n];
        double sum;
        for (int i = 0; i < n; i++) {
            sum = 0;
            for (int k = 0; k < i; k++) {
                sum += get(i, k) * y[k];
            }
            y[i] = r.get(i) - sum;
        }

        double[] x = new double[n];
        for (int i = n - 1; i >= 0; i--) {
            sum = 0;
            for (int k = i + 1; k < n; k++) {
                sum += get(i, k) * x[k];
            }
            x[i] = (y[i] - sum) / (get(i, i));
        }
        return Vector.of(x);
    }

    /**
     * Multiplication profile matrix on vector.
     *
     * @param vector given vector.
     * @return multiplied vector.
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


    @Override
    public String toString() {
        final String lineSeparator = System.lineSeparator();

        return "n=" + n + lineSeparator +
                "au=" + Arrays.toString(au) + lineSeparator +
                "al=" + Arrays.toString(al) + lineSeparator +
                "ia=" + Arrays.toString(ia) + lineSeparator +
                "di=" + Arrays.toString(di) + lineSeparator +
                "r=" + Arrays.toString(r) + lineSeparator;
    }
}
