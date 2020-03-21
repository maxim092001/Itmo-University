package queue;

import java.util.Arrays;
import java.util.Iterator;

public class ArrayQueue extends AbstractQueue implements Iterable<Object> {

    private int head = 0;
    private Object[] elements = new Object[2];

    @Override
    public void enqueueImpl(final Object element) {
        assert element != null : "Adding null element is not valid";
        if (super.size + 1 > elements.length) {
            extendArray();
        }
        elements[(head + super.size) % elements.length] = element;
    }

    private void extendArray() {
        if (head == 0) {
            elements = Arrays.copyOf(elements, elements.length + (elements.length >> 1));
        } else {
            Object[] newElements = new Object[elements.length + (elements.length >> 1)];
            if (super.size - head >= 0) {
                System.arraycopy(elements, head, newElements, 0, super.size - head);
            }

            if ((head + super.size) % elements.length + 1 >= 0) {
                System.arraycopy(elements, 0, newElements, -head + elements.length,
                        (head + super.size) % elements.length + 1);
            }
            head = 0;
            elements = newElements;
        }
    }

    @Override
    public Object dequeueImpl() {
        assert super.size > 0 : "Queue is empty";
        Object res = element();
        elements[head] = null;
        head = (head + 1) % elements.length;
        return res;
    }

    @Override
    public Object elementImpl() {
        assert super.size > 0 : "Queue is empty";
        return elements[head];
    }

    @Override
    protected Queue getQueue() {
        return new ArrayQueue();
    }

    @Override
    public void clearImpl() {
        elements = new Object[5];
        super.size = head = 0;
    }

    @Override
    public Iterator<Object> iterator() {
        return new ArrayQueueIterator();
    }

    private class ArrayQueueIterator implements Iterator<Object> {

        private int index = 0;
        private int head;

        public ArrayQueueIterator() {
            this.head = ArrayQueue.this.head;
        }

        @Override
        public boolean hasNext() {
            return index < size();
        }

        @Override
        public Object next() {
            index++;
            Object result = ArrayQueue.this.elements[this.head];
            this.head = (this.head + 1) % ArrayQueue.this.elements.length;
            return result;
        }
    }
}
