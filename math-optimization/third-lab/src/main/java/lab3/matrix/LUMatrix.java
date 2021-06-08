package lab3.matrix;

import lab3.Vector;

public class LUMatrix implements MatrixView {
    private final LUView luView;

    public LUMatrix(LUView luView) {
        this.luView = luView;
    }


    @Override
    public double get(int i, int j) {
        if (i <= j) {
            return luView.getU().get(i, j);
        } else {
            return luView.getL().get(i, j);
        }
    }

    @Override
    public int getN() {
        return luView.getL().getN();
    }

    @Override
    public Vector mul(Vector b) {
        throw new UnsupportedOperationException();
    }
}
