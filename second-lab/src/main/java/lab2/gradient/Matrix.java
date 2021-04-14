package lab2.gradient;

public class Matrix {
    private final double[][] a;
    private final boolean transposed;

    public Matrix(double[][] a) {
        if (a.length == 0) {
            throw new IllegalArgumentException();
        }

        int n = a[0].length;
        if (n == 0) {
            throw new IllegalArgumentException();
        }

        for (double[] line : a) {
            if (line.length != n) {
                throw new IllegalArgumentException();
            }
        }

        this.a = a;
        this.transposed = false;
    }

    private Matrix(double[][] a, boolean transposed) {
        this.a = a;
        this.transposed = transposed;
    }

    public double get(int i, int j) {
        return (transposed ? a[j][i] : a[i][j]);
    }

    public int verticalLength() {
        return (transposed ? a[0].length : a.length);
    }

    public int horizontalLength() {
        return (transposed ? a.length : a[0].length);
    }

    public Matrix mul(Number alpha) {
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

    public Matrix add(Matrix right) {
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

    public Matrix mul(Matrix right) {
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

    public Matrix transpose() {
        return new Matrix(a, !transposed);
    }

    public static Matrix mul(Matrix a, Matrix b) {
        return a.mul(b);
    }
}
