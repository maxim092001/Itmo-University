package lab2.gradient;

import java.util.Arrays;
import java.util.stream.IntStream;

/**
 * One dimensional matrix representation.
 */
public class Vector extends Matrix {
    /**
     * Vector constructor.
     *
     * @param vector given args.
     */
    public Vector(final double... vector) {
        super(prepareMatrix(vector));
    }

    /**
     * Create vector from matrix.
     *
     * @param matrix given matrix.
     */
    public Vector(final double[][] matrix) {
        super(matrix);
        if (matrix.length == 1) {
            throw new IllegalArgumentException("Vector is one dimension matrix only");
        }
    }

    /**
     * Get vectors element by index.
     *
     * @param i given index
     * @return element.
     */
    public double get(final int i) {
        return get(i, 0);
    }

    /**
     * Vectors length.
     *
     * @return vectors length.
     */
    public int length() {
        return verticalLength();
    }

    /**
     * Multiply vector by scalar number.
     *
     * @param alpha given scalar number.
     * @return multiplied vector.
     */
    public Vector mul(final Number alpha) {
        return (Vector) super.mul(alpha);
    }

    /**
     * Add two vectors.
     *
     * @param right given vector.
     * @return added vector.
     */
    public Vector add(final Vector right) {
        return (Vector) super.add(right);
    }

    /**
     * Subtract two vectors.
     *
     * @param right given vector
     * @return subtracted vector.
     */
    public Vector sub(final Vector right) {
        return (Vector) super.add(right.mul(-1.0));
    }

    /**
     * Multiply vector on another vector.
     *
     * @param right given vector.
     * @return multiplied vector.
     */
    public double scalarMul(final Vector right) {
        return transpose().mul(right).get(0, 0);
    }

    /**
     * Square rate of vector.
     *
     * @return square rate.
     */
    public double sqrRate() {
        double sum = 0.0;
        for (int i = 0; i < length(); i++) {
            sum += get(i) * get(i);
        }
        return sum;
    }

    /**
     * Vectors rate.
     *
     * @return rate.
     */
    public double rate() {
        return Math.sqrt(sqrRate());
    }

    /**
     * Create matrix from vector.
     * @param vector given vector.
     * @return matrix with one row.
     */
    private static double[][] prepareMatrix(double[] vector) {
        if (vector.length == 0) {
            throw new IllegalArgumentException();
        }
        double[][] a = new double[vector.length][1];
        IntStream.range(0, vector.length).forEach(i -> a[i][0] = vector[i]);
        return a;
    }

    @Override
    public String toString() {
        return "Vector{} " + super.toString();
    }
}
