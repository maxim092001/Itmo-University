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
}
