package lab3.matrix;

public class LUView {
    private final MatrixView a;

    public LUView(MatrixView a) {
        this.a = a;
    }

    public MatrixView getL() {
        return new MatrixView() {
            @Override
            public double get(int i, int j) {
                return i < j ? 0.0 : (i == j ? 1.0 : a.get(i, j));
            }

            @Override
            public int getN() {
                return a.getN();
            }
        };
    }

    public MatrixView getU() {
        return new MatrixView() {
            @Override
            public double get(int i, int j) {
                return (i > j ? 0.0 : a.get(i, j));
            }

            @Override
            public int getN() {
                return a.getN();
            }
        };
    }
}
