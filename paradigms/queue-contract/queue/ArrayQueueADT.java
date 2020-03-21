package queue;

import java.util.Arrays;

// Let Q - ArrayQueueADT
// Inv: |Q| >= 0 && for i = 1 ... |Q| : Q[i] != null
public class ArrayQueueADT {

    private int size = 0;
    private int head = 0;
    private Object[] elements = new Object[2];

    // Pre: queue != null && element != null
    // Post: |Q'| = |Q| + 1
    // forall i = 1 ... |Q| : Q'[i] = Q[i]
    // Q'[|Q'|] = element
    public static void enqueue(ArrayQueueADT queue, Object element) {
        assert queue != null : "Null queue is not valid";
        assert element != null : "Adding null element is not valid";
        if (queue.size + 1 > queue.elements.length) {
            extendArray(queue);
        }
        queue.elements[(queue.head + queue.size) % queue.elements.length] = element;
        queue.size++;
    }

    // Pre: queue != null
    // Q' = Q && |Q'| >= |Q|
    private static void extendArray(ArrayQueueADT queue) {
        if (queue.head == 0) {
            queue.elements = Arrays.copyOf(queue.elements, queue.elements.length + (queue.elements.length >> 1));
        } else {
            Object[] newElements = new Object[queue.elements.length + (queue.elements.length >> 1)];
            if (queue.size - queue.head >= 0) {
                System.arraycopy(queue.elements, queue.head, newElements, 0, queue.size - queue.head);
            }

            if ((queue.head + queue.size) % queue.elements.length + 1 >= 0) {
                System.arraycopy(queue.elements, 0, newElements, -queue.head + queue.elements.length,
                        (queue.head + queue.size) % queue.elements.length + 1);
            }
            queue.head = 0;
            queue.elements = newElements;
        }
    }

    // Pre: queue != null
    // Post: |Q|
    // |Q'| = |Q| && Q' = Q
    public static int size(ArrayQueueADT queue) {
        assert queue != null : "Null queue is not valid";
        return queue.size;
    }


    // Pre: queue != null
    // Post: Res = (size == 0)
    // Q' = Q
    public static boolean isEmpty(ArrayQueueADT queue) {
        assert queue != null : "Null queue is not valid";
        return queue.size == 0;
    }

    // Pre: queue != null && |Q| > 0
    // Post: Q[1]
    // |Q'| = |Q| - 1
    // forall i = 2 ... |Q| : Q'[i] = Q[i]
    public static Object dequeue(ArrayQueueADT queue) {
        assert queue != null : "Null queue is not valid";
        assert queue.size > 0 : "Queue is empty";
        Object res = element(queue);
        queue.elements[queue.head] = null;
        queue.head = (queue.head + 1) % queue.elements.length;
        queue.size--;
        return res;
    }

    // Pre: queue != null && |Q| > 0
    // Post: Q[1] from Q
    // Q' = Q
    public static Object element(ArrayQueueADT queue) {
        assert queue != null : "Null queue is not valid";
        assert queue.size > 0 : "Queue is empty";
        return queue.elements[queue.head];
    }

    // Pre: queue != null
    // Post: |Q'| = 0 && Q = {}
    public static void clear(ArrayQueueADT queue) {
        assert queue != null : "Null queue is not valid";
        queue.elements = new Object[5];
        queue.size = queue.head = 0;
    }

    // Pre: queue != null && element != null
    // Post: |Q'| = |Q| + 1
    // Q[1] = element
    // forall i = 2 ... |Q'| : Q'[i] = Q[i - 1]
    public static void push(ArrayQueueADT queue, Object element) {
        assert queue != null : "Null queue is not valid";
        assert element != null : "Adding null element is not valid";
        if (queue.size + 1 > queue.elements.length) {
            extendArray(queue);
        }
        queue.elements[((queue.head - 1) + queue.elements.length) % queue.elements.length] = element;
        queue.head = ((queue.head - 1) + queue.elements.length) % queue.elements.length;
        queue.size++;
    }

    // Pre: queue != null && |Q| > 0
    // Post: Q[|Q|] from Q
    // Q' = Q
    public static Object peek(ArrayQueueADT queue) {
        assert queue != null : "Null queue is not valid";
        assert queue.size > 0 : "Queue is empty";
        return queue.elements[((queue.head + queue.size - 1) + queue.elements.length) % queue.elements.length];
    }

    // Pre: queue != null
    // Post: Q[|Q|] from Q
    // |Q'| = |Q| - 1
    // forall i = 1 ... |Q'| : Q'[i] = Q[i]
    public static Object remove(ArrayQueueADT queue) {
        assert queue != null : "Null queue is not valid";
        assert queue.size > 0 : "Queue is empty";
        Object res = peek(queue);
        queue.elements[((queue.head + queue.size - 1) + queue.elements.length) % queue.elements.length] = null;
        queue.size--;
        return res;
    }
}
