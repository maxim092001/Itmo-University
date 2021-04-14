package lab2.gradient;

public class Vector extends Matrix {
    public Vector(final double... vector) {
        super(prepareMatrix(vector));
    }

    public Vector(final double[][] matrix) {
        super(matrix);
    }

    public double get(final int i) {
        return get(i, 0);
    }

    public int length() {
        return verticalLength();
    }

    public Vector mul(final Number alpha) {
        return (Vector) super.mul(alpha);
    }

    public Vector add(final Vector right) {
        return (Vector) super.add(right);
    }

    public Vector sub(final Vector right) {
        return (Vector) super.add(right.mul(-1.0));
    }

    public double scalarMul(final Vector right) {
        return transpose().mul(right).get(0, 0);
    }

    public double sqrNorm() {
        double sum = 0.0;
        for (int i = 0; i < length(); i++) {
            sum += get(i) * get(i);
        }
        return sum;
    }

    public double rate() {
        return Math.sqrt(sqrNorm());
    }

    private static double[][] prepareMatrix(double[] vector) {
        if (vector.length == 0) {
            throw new IllegalArgumentException();
        }

        double[][] a = new double[vector.length][1];
        for (int i = 0; i < vector.length; i++) {
            a[i][0] = vector[i];
        }
        return a;
    }

    @Override
    public String toString() {
        return "Vector{} " + super.toString();
    }
}
