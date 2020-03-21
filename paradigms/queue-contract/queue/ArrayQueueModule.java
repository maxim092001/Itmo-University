package queue;

import java.util.Arrays;

// Let Q - ArrayQueueModule
// Inv: |Q| >= 0 && for i = 1 ... |Q| : Q[i] != null
public class ArrayQueueModule {

    private static int size = 0;
    private static int head = 0;
    private static Object[] elements = new Object[2];

    // Pre: element != null
    // Post: |Q'| = |Q| + 1
    // forall i = 1 ... |Q| : Q'[i] = Q[i]
    // Q'[|Q'|] = element
    public static void enqueue(Object element) {
        assert element != null : "Adding null element is not valid";
        if (size + 1 > elements.length) {
            extendArray();
        }
        elements[(head + size) % elements.length] = element;
        size++;
    }

    // Pre: true
    // Q' = Q && |Q'| >= |Q|
    private static void extendArray() {
        if (head == 0) {
            elements = Arrays.copyOf(elements, elements.length + (elements.length >> 1));
        } else {
            Object[] newElements = new Object[elements.length + (elements.length >> 1)];
            if (size - head >= 0) {
                System.arraycopy(elements, head, newElements, 0, size - head);
            }
            if ((head + size) % elements.length + 1 >= 0) {
                System.arraycopy(elements, 0, newElements, -head + elements.length,
                        (head + size) % elements.length + 1);
            }
            head = 0;
            elements = newElements;
        }
    }

    // Pre: true
    // Post: |Q|
    // |Q'| = |Q| && Q' = Q
    public static int size() {
        return size;
    }

    // Pre: true
    // Post: Res = (size == 0)
    // Q' = Q
    public static boolean isEmpty() {
        return size == 0;
    }

    // Pre: |Q| > 0
    // Post: Q[1]
    // |Q'| = |Q| - 1
    // forall i = 2 ... |Q| : Q'[i] = Q[i]
    public static Object dequeue() {
        assert size > 0 : "Queue is empty";
        Object res = element();
        elements[head] = null;
        head = (head + 1) % elements.length;
        size--;
        return res;
    }

    // Pre: |Q| > 0
    // Post: Q[1] from Q
    // Q' = Q
    public static Object element() {
        assert size > 0 : "Queue is empty";
        return elements[head];
    }

    // Post: |Q'| = 0 && Q = {}
    public static void clear() {
        elements = new Object[5];
        size = head = 0;
    }

    // Pre: element != null
    // Post: |Q'| = |Q| + 1
    // Q[1] = element
    // forall i = 2 ... |Q'| : Q'[i] = Q[i - 1]
    public static void push(Object element) {
        assert element != null : "Adding null element is not valid";
        if (size + 1 > elements.length) {
            extendArray();
        }
        elements[((head - 1) + elements.length) % elements.length] = element;
        head = ((head - 1) + elements.length) % elements.length;
        size++;
    }

    // Pre: |Q| > 0
    // Post: Q[|Q|] from Q
    // Q' = Q
    public static Object peek() {
        assert size > 0 : "Queue is empty";
        return elements[((head + size - 1) + elements.length) % elements.length];
    }

    // Post: Q[|Q|] from Q
    // |Q'| = |Q| - 1
    // forall i = 1 ... |Q'| : Q'[i] = Q[i]
    public static Object remove() {
        assert size > 0 : "Queue is empty";
        Object res = peek();
        elements[((head + size - 1) + elements.length) % elements.length] = null;
        size--;
        return res;
    }
}
