package lab3.matrix;

public interface MatrixView {
    /**
     * Gets element from profile matrix.
     *
     * @param i row index.
     * @param j column index.
     * @return elements value.
     */
    double get(final int i, final int j);

    int getN();
}
