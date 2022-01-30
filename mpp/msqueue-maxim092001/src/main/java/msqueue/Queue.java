package msqueue;

/**
 * Queue interface.
 *
 * @author Nikita Koval
 */
public interface Queue {

    /**
     * Inserts the specified element into this queue
     *
     * @param x the element to add
     */
    void enqueue(int x);

    /**
     * Retrieves and removes the head of this queue,
     * or returns {@link Integer#MIN_VALUE} if this queue is empty
     */
    int dequeue();

    /**
     * Retrieves, but does not remove, the head of this queue,
     * or returns {@link Integer#MIN_VALUE} if this queue is empty
     */
    int peek();
}
