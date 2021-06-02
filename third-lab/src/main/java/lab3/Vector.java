package lab3;

import java.util.Arrays;

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
        for (int i = 0; i < n; i++) {
            vector[i] -= a.get(i);
        }
        return this;
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
//            res += Math.abs(a);
        }
        return Math.sqrt(res);
//        return res;
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

    @Override
    public String toString() {
        return "Vector{" +
                "vector=" + Arrays.toString(vector) +
                ", n=" + n +
                '}';
    }
}
