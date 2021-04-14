package lab2.gradient;

import java.util.function.Function;

public class QuadraticFunction implements Function<Vector, Double> {
    private final Matrix a;
    private final Vector b;
    private final double c;

    public QuadraticFunction(Matrix a, Vector b, double c) {
        int n = a.verticalLength();
        int m = a.horizontalLength();

        if (n != m || n != b.length()) {
            throw new IllegalArgumentException();
        }

        double[][] aSymmetric = new double[n][n];
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                aSymmetric[i][j] = a.get(i, j) + a.get(j, i);
            }
        }

        this.a = new Matrix(aSymmetric);
        this.b = b;
        this.c = c;
    }

    @Override
    public Double apply(Vector x) {
        return 0.5 * ((Vector) a.mul(x)).scalarMul(x) + b.scalarMul(x) + c;
    }

    public Matrix getA() {
        return a;
    }

    public Vector getB() {
        return b;
    }

    public double getC() {
        return c;
    }

    public Vector gradient(Vector x) {
        return ((Vector) getA().mul(x)).add(getB());
    }

    public Vector antigradient(Vector x) {
        return gradient(x).mul(-1);
    }

    public int dimensions() {
        return a.verticalLength();
    }

    // A x^2 + B xy + C y^2 + Dx + Ey + F
    public static QuadraticFunction from2d(double a, double b, double c, double d, double e, double f) {
        double[][] A = new double[][] {
                {a, b},
                {0.0, c}
        };
        double[] B = new double[] {d, e};
        return new QuadraticFunction(new Matrix(A), new Vector(B), f);
    }

    public static void main(String[] args) {
        // Пример из пояснений
        QuadraticFunction f1 = from2d(64, 126, 64, -10, 30, 13);
        System.out.println(f1.apply(new Vector(1, 1)));
    }
}
