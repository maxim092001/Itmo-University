package lab4.utils;

import lab4.matrix.Vector;

public class IterationStep {
    private final double alpha;
    private final Vector x;
    private final Vector pk;
    private final double fX;

    public IterationStep(final double alpha, final Vector x, final Vector pk, final double fX) {
        this.alpha = alpha;
        this.x = x;
        this.pk = pk;
        this.fX = fX;
    }

    public double getAlpha() {
        return alpha;
    }

    public Vector getX() {
        return x;
    }

    public Vector getPk() {
        return pk;
    }

    public double getfX() {
        return fX;
    }
}
