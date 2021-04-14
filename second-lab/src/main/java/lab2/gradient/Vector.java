package lab2.gradient;

public class Vector extends Matrix {
    public Vector(double... vector) {
        super(prepareMatrix(vector));
    }

    public Vector(double[][] matrix) {
        super(matrix);
    }

    public double get(int i) {
        return get(i, 0);
    }

    public int length() {
        return verticalLength();
    }

    public Vector mul(Number alpha) {
        return (Vector) super.mul(alpha);
    }

    public double scalarMul(Vector right) {
        return transpose().mul(right).get(0, 0);
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

}
