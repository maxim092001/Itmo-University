package linked_list_set;

/**
 * Stack interface.
 */
public interface Set {

    /**
     * Adds the specified element to this set if it is not already present
     * (optional operation).
     *
     * @param x element to be added to this set
     * @return <tt>true</tt> if this set did not already contain the specified
     * element
     */
    boolean add(int x);

    /**
     * Removes the specified element from this set if it is present
     * (optional operation).
     *
     * @param x object to be removed from this set, if present
     * @return <tt>true</tt> if this set contained the specified element
     */
    boolean remove(int x);

    /**
     * Returns <tt>true</tt> if this set contains the specified element.
     *
     * @param x element whose presence in this set is to be tested
     * @return <tt>true</tt> if this set contains the specified element
     */
    boolean contains(int x);
}
