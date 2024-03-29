package lab3.matrix;

import lab3.Vector;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Representation of profile matrix.
 */
public class ProfileMatrix implements Matrix<ProfileMatrix> {
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

    @SuppressWarnings("CopyConstructorMissesField")
    public ProfileMatrix(ProfileMatrix another) {
        this(
                another.n,
                Arrays.copyOf(another.au, another.au.length),
                Arrays.copyOf(another.al, another.al.length),
                Arrays.copyOf(another.ia, another.ia.length),
                Arrays.copyOf(another.di, another.di.length),
                (another.r == null ? null : Arrays.copyOf(another.r, another.r.length))
        );
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

    @Override
    public int getN() {
        return n;
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
     * Multiplication profile matrix on vector.
     *
     * @param vector given vector.
     * @return multiplied vector.
     */
    public Vector mul(final Vector vector) {
        final double[] result = new double[n];
        int n = getN();
        for (int i = 0; i < n; i++) {
            result[i] = di[i] * vector.get(i);
        }

        for (int i = 1; i < n; ++i) {
            for (int j = ia[i]; j < ia[i + 1]; ++j) {
                int k = j - ia[i];
                int profileStart = i - ia[i + 1] + ia[i];
                result[i] += al[j - 1] * vector.get(profileStart + k);
                result[profileStart + k] += au[j - 1] * vector.get(i);
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

    @Override
    public ProfileMatrix copy() {
        return new ProfileMatrix(this);
    }

    public static void main(String[] args) {
        ProfileMatrix a = new ProfileMatrix(new double[][]{
                {1.5, 4.8, 1.3, 7.5},
                {9.1, 25.1, 0.9, 5.4},
                {78.4, 1.0, 1.0, 98.1},
                {-45.0, 14.5, 6.5, 7.8}
        });

        Vector vector = Vector.of(9.0, 8.0, 7.0, 6.0);

        System.out.println(a.mul(vector).toString());
        //Vector{vector=[106.0, 321.4, 1309.1999999999998, -196.7], n=4}
    }
}
