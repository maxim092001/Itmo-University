package lab3.matrix;

/**
 * Matrix interface.
 */
public interface Matrix {

    /**
     * Gets element from profile matrix.
     *
     * @param i row index.
     * @param j column index.
     * @return elements value.
     */
    double get(final int i, final int j);


    /**
     * Sets value to an element in profile matrix.
     *
     * @param i     row index.
     * @param j     column index.
     * @param value given value to set.
     */
    void set(final int i, final int j, final double value);

}
