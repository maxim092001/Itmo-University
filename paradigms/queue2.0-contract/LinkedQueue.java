package queue;

import java.util.Iterator;

public class LinkedQueue extends AbstractQueue implements Iterable<Object> {

    public Queue getQueue() {
        return new LinkedQueue();
    }

    private Node head;
    private Node tail;

    @Override
    protected void clearImpl() {
        head = tail = null;
    }

    @Override
    protected Object elementImpl() {
        return head.getValue();
    }

    @Override
    protected Object dequeueImpl() {
        Object result = head.getValue();
        head = head.getNext();
        return result;
    }

    @Override
    protected void enqueueImpl(Object element) {
        Node newNode = new Node(element, null);
        Node oldTail = tail;
        tail = newNode;
        if (super.isEmpty()) {
            head = newNode;
        } else {
            oldTail.setNext(newNode);
        }
    }

    @Override
    public Iterator<Object> iterator() {
        return new LinkedQueueIterator(this.head);
    }

    private static class LinkedQueueIterator implements Iterator<Object> {

        Node cur;

        public LinkedQueueIterator(Node head) {
            cur = head;
        }

        @Override
        public boolean hasNext() {
            return cur != null;
        }

        @Override
        public Object next() {
            Object result = cur.getValue();
            cur = cur.getNext();
            return result;
        }
    }
}
