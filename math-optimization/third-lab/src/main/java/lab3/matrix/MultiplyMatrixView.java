package lab3.matrix;

public class MultiplyMatrixView implements MatrixView {
    private final MatrixView l, r;

    public MultiplyMatrixView(MatrixView l, MatrixView r) {
        assert l.getN() == r.getN();
        this.l = l;
        this.r = r;
    }

    @Override
    public double get(int i, int j) {
        double sum = 0.0;
        for (int k = 0; k < getN(); ++k) {
            sum += l.get(i, k) * r.get(k, j);
        }
        return sum;
    }

    @Override
    public int getN() {
        return l.getN();
    }
}
