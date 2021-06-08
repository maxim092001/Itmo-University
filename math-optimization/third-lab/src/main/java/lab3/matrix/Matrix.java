package lab3.matrix;

/**
 * Matrix interface.
 */
public interface Matrix<T> extends MatrixView, Copyable<T> {
    /**
     * Sets value to an element in profile matrix.
     *
     * @param i     row index.
     * @param j     column index.
     * @param value given value to set.
     */
    void set(final int i, final int j, final double value);

    default LUView luDecomposition() {
        double sum;
        int n = getN();
        for (int i = 0; i < n; i++) {
            for (int j = i; j < n; j++) {
                sum = 0;
                for (int k = 0; k < i; k++) {
                    sum += get(i, k) * get(k, j);
                }
                set(i, j, get(i, j) - sum);
            }
            // L
            for (int j = i + 1; j < n; j++) {
                sum = 0;
                for (int k = 0; k < i; k++) {
                    sum += get(j, k) * get(k, i);
                }
                set(j, i, (1.0 / get(i, i)) * (get(j, i) - sum));
            }
        }
        return new LUView(this);
    }
}
