package msqueue;

import kotlinx.atomicfu.AtomicRef;

public class MSQueue implements Queue {
    private final AtomicRef<Node> head;
    private final AtomicRef<Node> tail;

    public MSQueue() {
        final Node dummy = new Node(0);
        this.head = new AtomicRef<>(dummy);
        this.tail = new AtomicRef<>(dummy);
    }

    @Override
    public void enqueue(final int x) {
        final Node newTail = new Node(x);
        while (true) {
            final Node tempTailValue = tail.getValue();
            if (tempTailValue.next.compareAndSet(null, newTail)) {
                tail.compareAndSet(tempTailValue, newTail);
                break;
            } else {
                tail.compareAndSet(tempTailValue, tempTailValue.next.getValue());
            }
        }
    }

    @Override
    public int dequeue() {
        while (true) {
            final Node tempHead = head.getValue();
            final Node tempTail = tail.getValue();
            final Node next = tempHead.next.getValue();
            if (tempHead == tempTail) {
                if (next == null) {
                    return Integer.MIN_VALUE;
                }
                tail.compareAndSet(tempTail, next);
            } else {
                if (head.compareAndSet(tempHead, next)) {
                    return next.x;
                }
            }
        }

    }

    @Override
    public int peek() {
        final Node tempHead = head.getValue();
        final Node tempTail = tail.getValue();
        final Node next = tempHead.next.getValue();
        if (tempHead == tempTail && next == null) {
            return Integer.MIN_VALUE;
        }
        return next.x;
    }

    private static class Node {
        final int x;
        AtomicRef<Node> next = new AtomicRef<>(null);

        Node(final int x) {
            this.x = x;
        }
    }
}