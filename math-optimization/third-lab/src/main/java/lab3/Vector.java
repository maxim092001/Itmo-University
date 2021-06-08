package lab3;

import lab3.matrix.Copyable;

import java.util.Arrays;
import java.util.function.DoubleUnaryOperator;
import java.util.stream.DoubleStream;

/**
 * Util class for one dimension matrix.
 */
public class Vector implements Copyable<Vector> {

    /**
     * Vector.
     */
    private final double[] vector;

    /**
     * Size.
     */
    private final int n;

    private Vector(final double... vector) {
        this.vector = vector;
        this.n = vector.length;
    }

    public static Vector of(final double... vector) {
        return new Vector(vector);
    }

    public static Vector zero(int n) {
        return Vector.of(new double[n]);
    }

    public static Vector natural(int n) {
        double[] v = new double[n];
        for (int i = 0; i < n; i++) {
            v[i] = i + 1;
        }
        return Vector.of(v);
    }

    /**
     * Size of current vector.
     *
     * @return size.
     */
    public int size() {
        return n;
    }

    /**
     * Get element by index.
     *
     * @param i index.
     * @return value on index.
     */
    public double get(final int i) {
        return vector[i];
    }

    /**
     * Sets value on given index.
     *
     * @param i     index.
     * @param value value
     */
    public void set(final int i, final double value) {
        vector[i] = value;
    }

    public Vector sub(final Vector a) {
        double[] result = new double[n];
        for (int i = 0; i < n; i++) {
            result[i] = vector[i] - a.get(i);
        }
        return Vector.of(result);
    }

    /**
     * Norm of current vector.
     *
     * @return norm.
     */
    public double norm() {
        double res = 0;
        for (int i = 0; i < n; i++) {
            double a = get(i);
            res += a * a;
        }
        return Math.sqrt(res);
    }

    /**
     * Swaps two elements in vector.
     *
     * @param i first index.
     * @param j second index.
     */
    public void swap(final int i, final int j) {
        double tmp = vector[i];
        vector[i] = vector[j];
        vector[j] = tmp;
    }

    public Vector mul(double t) {
        double[] result = new double[n];
        for (int i = 0; i < n; i++) {
            result[i] = get(i) * t;
        }
        return Vector.of(result);
    }

    public double scalarMul(Vector b) {
        assert b.size() == n;
        double result = 0.0;
        for (int i = 0; i < n; i++) {
            result += get(i) * b.get(i);
        }
        return result;
    }

    public Vector add(Vector b) {
        assert b.size() == n;
        double[] result = new double[n];
        for (int i = 0; i < n; i++) {
            result[i] = get(i) + b.get(i);
        }
        return Vector.of(result);
    }

    /**
     * Copies current vector.
     *
     * @return new copy.
     */
    public Vector copy() {
        return Vector.of(Arrays.copyOf(vector, n));
    }

    @Override
    public String toString() {
        return "Vector{" +
                "vector=" + Arrays.toString(vector) +
                ", n=" + n +
                '}';
    }
}
