package lab3.matrix;

import lab3.Vector;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class SparseMatrix implements Matrix<SparseMatrix> {
    private static final double EPS = 1e-12;

    private final int n;
    private final double[] di, al, au;
    private final int[] ia, ja;

    public SparseMatrix(int n, double[] di, double[] al, double[] au, int[] ia, int[] ja) {
        this.n = n;
        this.di = di;
        this.al = al;
        this.au = au;
        this.ia = ia;
        this.ja = ja;
    }

    public SparseMatrix(double[][] a) {
        List<Double> al = new ArrayList<>(), au = new ArrayList<>();
        List<Integer> ia = new ArrayList<>(List.of(1)), ja = new ArrayList<>();
        n = a.length;
        di = new double[n];
        for (int i = 0; i < n; i++) {
            assert a[i].length == n;
            di[i] = a[i][i];
            int nonZeroCount = 0;
            for (int j = 0; j < i; j++) {
                if ((Math.abs(a[i][j]) > EPS || Math.abs(a[j][i]) > EPS)) {
                    nonZeroCount++;

                    ja.add(j);
                    al.add(a[i][j]);
                    au.add(a[j][i]);
                }
            }

            ia.add(ia.get(ia.size() - 1) + nonZeroCount);
        }

        this.au = au.stream().mapToDouble(i -> i).toArray();
        this.al = al.stream().mapToDouble(i -> i).toArray();
        this.ia = ia.stream().mapToInt(i -> i).toArray();
        this.ja = ja.stream().mapToInt(i -> i).toArray();
    }

    @Override
    public void set(int i, int j, double value) {
        throw new UnsupportedOperationException();
    }

    @Override
    public double get(int i, int j) {
        if (i == j) {
            return di[i];
        }

        if (i < j) {
            int index = Arrays.binarySearch(ja, ia[j] - 1, ia[j + 1] - 1, i);
            if (index < 0) {
                return 0;
            } else {
                return au[index];
            }
        } else {
            int index = Arrays.binarySearch(ja, ia[i] - 1, ia[i + 1] - 1, j);
            if (index < 0) {
                return 0;
            } else {
                return al[index];
            }
        }
    }

    @Override
    public int getN() {
        return n;
    }

    @Override
    public Vector mul(Vector b) {
        final double[] result = new double[n];
        int n = getN();
        for (int i = 0; i < n; i++) {
            result[i] = di[i] * b.get(i);
        }

        for (int i = 1; i < n; ++i) {
            for (int j = ia[i]; j < ia[i + 1]; ++j) {
                result[i] += al[j - 1] * b.get(ja[j - 1]);
                result[ja[j - 1]] += au[j - 1] * b.get(i);
            }
        }
        return Vector.of(result);
    }

    @Override
    public SparseMatrix copy() {
        return new SparseMatrix(
                n,
                Arrays.copyOf(di, di.length),
                Arrays.copyOf(al, al.length),
                Arrays.copyOf(au, au.length),
                Arrays.copyOf(ia, ia.length),
                Arrays.copyOf(ja, ja.length)
        );
    }

    public double[] getAl() {
        return Arrays.copyOf(al, al.length);
    }

    public double[] getAu() {
        return Arrays.copyOf(au, au.length);
    }

    private static void test(double[][] a) {
        SparseMatrix matrix = new SparseMatrix(a);
        FullMatrix full = new FullMatrix(a);
        if (!MatrixUtils.equals(matrix, full)) {
            System.err.println(MatrixUtils.toString(matrix));
        }
    }

    public static void main(String[] args) {
        test(new double[][]{
                {1, 2, 3},
                {4, 5, 6},
                {7, 8, 9}
        });
        test(new double[][]{
                {10, 2, 36, 6},
                {-4, 685, 68, 7},
                {7, 85, -89, 8},
                {70, 805, -809, 58}
        });

        SparseMatrix a = new SparseMatrix(new double[][]{
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
