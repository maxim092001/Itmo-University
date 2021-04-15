package lab2.gradient.utils;

import java.util.function.Function;

/**
 * Quadratic function representation.
 */
public class QuadraticFunction implements Function<Vector, Double> {

    /**
     * Matrix a. {@link Matrix}
     */
    private final Matrix a;

    /**
     * Matrix b. {@link Vector}
     */
    private final Vector b;

    /**
     * Coefficient c.
     */
    private final double c;

    /**
     * Quadratic function constructor.
     *
     * @param a matrix.
     * @param b vector
     * @param c coefficient.
     */
    public QuadraticFunction(final Matrix a, final Vector b, final double c) {
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

    /**
     * Get function result in point.
     *
     * @param point vector representation of point
     * @return function result.
     */
    @Override
    public Double apply(final Vector point) {
        return 0.5 * ((Vector) a.mul(point)).scalarMul(point) + b.scalarMul(point) + c;
    }

    /**
     * Getter for a.
     *
     * @return a.
     */
    public Matrix getA() {
        return a;
    }

    /**
     * Getter for b.
     *
     * @return b.
     */
    public Vector getB() {
        return b;
    }

    /**
     * Getter for c.
     *
     * @return c.
     */
    public double getC() {
        return c;
    }

    /**
     * Find gradient for function in point.
     *
     * @param point given point
     * @return gradient.
     */
    public Vector gradient(final Vector point) {
        return ((Vector) getA().mul(point)).add(getB());
    }

    /**
     * Find anti gradient for function in point
     *
     * @param point given point.
     * @return anti gradient.
     */
    public Vector antiGradient(final Vector point) {
        return gradient(point).mul(-1);
    }

    /**
     * Dimensions.
     *
     * @return dimensions.
     */
    public int dimensions() {
        return a.verticalLength();
    }

    @Override
    public String toString() {
        return "QuadraticFunction{" +
                "a=" + a +
                ", b=" + b +
                ", c=" + c +
                '}';
    }

    /**
     * Create quadratic function from 2d. {@code Ax^2 + Bxy + C y^2 + Dx + Ey + F}
     *
     * @param a coefficient.
     * @param b coefficient.
     * @param c coefficient.
     * @param d coefficient.
     * @param e coefficient.
     * @param f coefficient.
     * @return created quadratic function.
     */
    // A x^2 + B xy + C y^2 + Dx + Ey + F
    public static QuadraticFunction from2d(double a, double b, double c, double d, double e, double f) {
        double[][] A = new double[][]{
                {a, b},
                {0.0, c}
        };
        double[] B = new double[]{d, e};
        return new QuadraticFunction(new Matrix(A), new Vector(B), f);
    }

    public static void main(String[] args) {
        // Пример из пояснений
        QuadraticFunction f1 = from2d(64, 126, 64, -10, 30, 13);
        System.out.println(f1.apply(new Vector(1, 1)));
    }
}
