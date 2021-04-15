package lab2.gradient.utils;

public class DiagMatrix extends Matrix {
    public DiagMatrix(double... diag) {
        super(prepareMatrix(diag));
    }

    private static double[][] prepareMatrix(double[] diag) {
        if (diag.length == 0) {
            throw new IllegalArgumentException();
        }
        double[][] a = new double[1][diag.length];
        System.arraycopy(diag, 0, a[0], 0, diag.length);
        return a;
    }

    @Override
    public double get(int i, int j) {
        return (i != j ? 0 : super.get(0, j));
    }

    @Override
    public int verticalLength() {
        // It's not an error
        return super.horizontalLength();
    }

    @Override
    public Matrix mul(Number alpha) {
        return super.mul(alpha);
    }

    @Override
    public Matrix add(Matrix right) {
        if (right instanceof DiagMatrix) {
            DiagMatrix diagRight = (DiagMatrix) right;
            int n = verticalLength();

            if (diagRight.verticalLength() != n) {
                throw new IllegalArgumentException();
            }

            double[] result = new double[n];
            for (int i = 0; i < n; i++) {
                result[i] = get(i, i) + diagRight.get(i, i);
            }
            return new DiagMatrix(result);
        } else {
            return super.add(right);
        }
    }

    @Override
    public DiagMatrix mul(Matrix right) {
        int n = verticalLength();

        if (right.verticalLength() != n || right.horizontalLength() != n) {
            throw new IllegalArgumentException();
        }

        double[] result = new double[n];
        for (int i = 0; i < n; i++) {
            result[i] = get(i, i) * right.get(i, i);
        }
        return new DiagMatrix(result);
    }

    @Override
    public Matrix transpose() {
        return this;
    }

    @Override
    public String toString() {
        StringBuilder result = new StringBuilder("[").append(get(0, 0));
        for (int j = 1; j < verticalLength(); j++) {
            result.append(", ").append(get(j, j));
        }
        return result.append("]").toString();
    }

    public static void main(String[] args) {
        DiagMatrix a = new DiagMatrix(1.0, 2.0, 3.0);
        System.out.println(a.mul(new DiagMatrix(2, 4, 5)));
    }
}
