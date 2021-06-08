package lab4.matrix;

import java.util.Arrays;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/**
 * Util class for one dimension matrix.
 */
public class Vector {

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
        return add(a.mul(-1.0));
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

    /**
     * Copies current vector.
     *
     * @return new copy.
     */
    public Vector copy() {
        return Vector.of(Arrays.copyOf(vector, n));
    }

    /**
     * Add value to vectors element.
     * @param ind index.
     * @param value given value.
     * @return result vector.
     */
    public Vector add(final int ind, final double value) {
        double[] values = new double[n];
        values[ind] = value;
        for (int i = 0; i < n; i++) {
            values[i] += vector[i];
        }
        return new Vector(values);
    }

    /**
     * Multiply on scalar value.
     * @param val given value
     * @return multiplied vector.
     */
    public Vector mul(final double val) {
        double[] values = new double[n];
        for (int i = 0; i < n; i++) {
            values[i] = vector[i] * val;
        }
        return new Vector(values);
    }

    /**
     * Add vector to given vector
     * @param other given vector.
     * @return result vector.
     */
    public Vector add(final Vector other) {
        double[] values = new double[n];
        for (int i = 0; i < n; i++) {
            values[i] = vector[i] + other.get(i);
        }
        return new Vector(values);
    }

    /**
     * Scalar multiplication.
     * @param other given vector.
     * @return multiplied vector.
     */
    public double scalarMultiply(final Vector other) {
        return IntStream.range(0, n)
                .mapToDouble(i -> this.get(i) * other.get(i)).sum();
    }

    /**
     * Multiplied vector on transposed.
     * @param other given vector.
     * @return multiplication result.
     */
    public FullMatrix mulByTransposed(final Vector other) {
        double[][] result = new double[n][n];
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                result[i][j] = this.get(i) * other.get(j);
            }
        }
        return new FullMatrix(result);
    }

    @Override
    public String toString() {
        return "Vector{" +
                "vector=" + Arrays.toString(vector) +
                ", n=" + n +
                '}';
    }

    public String view() {
        return Arrays.toString(vector);
    }

    public String texView() {
        final var sb = new StringBuilder();
        sb.append(String.format("%.7f", vector[0]));
        Arrays.stream(vector).skip(1).forEach(i -> sb.append(" & ").append(String.format("%.7f", i)));
        return sb.toString();
    }

    public String wolframView(final Function<Vector, Double> f) {
        if (n == 1) {
            return "{" + Arrays.stream(vector).mapToObj(i -> String.format("%.7f, %.7f", i, f.apply(Vector.of(vector)))).collect(Collectors.joining(",")) + "}";
        } else {
            return "{" + Arrays.stream(vector).mapToObj(String::valueOf).collect(Collectors.joining(",")) + "}";
        }
    }
}
