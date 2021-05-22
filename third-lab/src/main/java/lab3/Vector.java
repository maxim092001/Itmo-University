package lab3;

public class Vector {
    private final double[] vector;
    private final int n;


    public Vector(final double[] vector, final int n) {
        this.vector = vector;
        this.n = n;
    }

    public int size() {
        return n;
    }

    public double get(final int i) {
        return vector[i];
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
}
