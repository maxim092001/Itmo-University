package lab3;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class ProfileMatrix {
    private static final double EPS = 1e-5;

    /**
     * Matrix dimension.
     */
    final int n;

    /**
     * Matrix elements starting from first non-zero above main diagonal.
     */
    final double[] au;

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
            final double[] di
    ) {
        this.n = n;
        this.au = au;
        this.al = al;
        this.ia = ia;
        this.di = di;
    }

    private ProfileMatrix(final double[][] matrix) {
        di = new double[matrix.length];
        List<Double> au = new ArrayList<>();
        List<Double> al = new ArrayList<>();
        List<Integer> ia = new ArrayList<>(List.of(1, 1));

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
        }

        this.n = matrix.length;
        this.au = au.stream().mapToDouble(i -> i).toArray();
        this.al = al.stream().mapToDouble(i -> i).toArray();
        this.ia = ia.stream().mapToInt(i -> i).toArray();
    }

    public static ProfileMatrix of(
            final int n,
            final double[] au,
            final double[] al,
            final int[] ia,
            final double[] di
    ) {
        return new ProfileMatrix(n, au, al, ia, di);
    }

    public static ProfileMatrix of(final int n) {
        return new ProfileMatrix(n, new double[n], new double[n], new int[n], new double[n]);
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
            x[i] = (1.0 / get(i, i)) * (y[i] - sum);
        }
        return new Vector(x, n);
    }


    @Override
    public String toString() {
        final String lineSeparator = System.lineSeparator();

        return "n=" + n + lineSeparator +
                "au=" + Arrays.toString(au) + lineSeparator +
                "al=" + Arrays.toString(al) + lineSeparator +
                "ia=" + Arrays.toString(ia) + lineSeparator +
                "di=" + Arrays.toString(di) + lineSeparator;
    }
}
