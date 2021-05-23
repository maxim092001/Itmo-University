package lab3;

import java.util.Arrays;

public class Vector {
    private final double[] vector;
    private final int n;


    public Vector(final int n, final double... vector) {
        this.vector = vector;
        this.n = n;
    }

    public int size() {
        return n;
    }

    public double get(final int i) {
        return vector[i];
    }

    public void set(final int i, final double x) {
        vector[i] = x;
    }

    public void sub(final Vector a) {
        for (int i = 0; i < n; i++) {
            vector[i] -= a.get(i);
        }
    }

    public double norm() {
        double res = 0;
        for (int i = 0; i < n; i++) {
            double a = get(i);
            res += a * a;
        }
        return Math.sqrt(res);
    }

    public void swap(int i, int j) {
        double tmp = vector[i];
        vector[i] = vector[j];
        vector[j] = tmp;
    }

    public Vector copy() {
        return new Vector(n, Arrays.copyOf(vector, n));
    }

    @Override
    public String toString() {
        return "Vector{" +
                "vector=" + Arrays.toString(vector) +
                ", n=" + n +
                '}';
    }
}
