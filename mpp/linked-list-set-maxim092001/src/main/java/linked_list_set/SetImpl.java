package linked_list_set;

import kotlinx.atomicfu.AtomicRef;

public class SetImpl implements Set {

    private static Node castToRemoved(final Node node) {
        return ((Removed) node).next;
    }

    private static class Node {
        final AtomicRef<Node> next;
        final int x;

        public Node() {
            this.next = new AtomicRef<>(null);
            this.x = 0;
        }

        Node(final int x, final Node next) {
            this.next = new AtomicRef<>(next);
            this.x = x;
        }

        public Node removeNext() {
            Node next = this.next.getValue();
            if (next instanceof Removed) {
                next = castToRemoved(next);
            }
            return next;
        }
    }

    private static class Window {
        final Node cur;
        final Node next;

        public Window(final Node cur, final Node next) {
            this.cur = cur;
            this.next = next;
        }
    }

    private static class Removed extends Node {
        final Node next;

        private Removed(final Node next) {
            this.next = next;
        }

        public static Removed of(final Node next) {
            return new Removed(next);
        }

    }

    private final Node head = new Node(Integer.MIN_VALUE, new Node(Integer.MAX_VALUE, null));

    /**
     * Returns the {@link Window}, where cur.x < x <= next.x
     */
    private Window findWindow(final int x) {
        while (true) {
            Node current = head;
            Node next = current.removeNext();
            while (next.x < x) {
                final Node newNext = next.next.getValue();
                if (newNext instanceof Removed) {
                    if (!current.next.compareAndSet(next, castToRemoved(newNext))) {
                        current = head;
                        next = current.removeNext();
                    } else {
                        next = castToRemoved(newNext);
                    }
                } else {
                    current = next;
                    next = current.removeNext();
                }
            }
            final Node newNext = next.next.getValue();
            if (newNext instanceof Removed) {
                current.next.compareAndSet(next, castToRemoved(newNext));
            } else {
                return new Window(current, next);
            }
        }
    }

    @Override
    public boolean add(final int x) {
        while (true) {
            final Window w = findWindow(x);
            if (w.next.x == x) {
                return false;
            }
            final Node node = new Node(x, w.next);
            if (w.cur.next.compareAndSet(w.next, node)) {
                return true;
            }
        }
    }

    @Override
    public boolean remove(final int x) {
        while (true) {
            final Window w = findWindow(x);
            if (w.next.x != x) {
                return false;
            }
            final Node newNext = w.next.removeNext();
            final Node removedNewNext = Removed.of(newNext);
            if (w.next.next.compareAndSet(newNext, removedNewNext)) {
                w.cur.next.compareAndSet(w.next, newNext);
                return true;
            }
        }
    }

    @Override
    public boolean contains(final int x) {
        return findWindow(x).next.x == x;
    }
}