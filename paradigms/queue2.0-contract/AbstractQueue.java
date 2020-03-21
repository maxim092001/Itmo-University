package queue;

import java.lang.reflect.InvocationTargetException;
import java.util.function.Function;
import java.util.function.Predicate;

public abstract class AbstractQueue implements Queue, Iterable<Object> {

    protected int size = 0;

    @Override
    public Queue filter(Predicate<Object> p) {
        assert p != null : "Predicate cannot be null";
        Queue queue = this.getQueue();

        for (Object element : this) {
            if (p.test(element)) {
                queue.enqueue(element);
            }
        }
        return queue;
    }

    @Override
    public Queue map(Function<Object, Object> f) {
        assert f != null : "Function cannot be null";

        Queue queue = this.getQueue();

        for (Object element : this) {
            queue.enqueue(f.apply(element));
        }

        return queue;
    }

    protected Queue getQueue() {
        return this.getQueue();
    }

    protected abstract void clearImpl();

    protected abstract Object elementImpl();

    protected abstract Object dequeueImpl();

    protected abstract void enqueueImpl(Object element);


    @Override
    public void enqueue(Object element) {
        assert element != null : "Adding null element is not valid";
        enqueueImpl(element);
        this.size++;
    }


    @Override
    public Object dequeue() {
        assert this.size > 0 : "Queue is empty";
        Object result = dequeueImpl();
        this.size--;
        return result;
    }


    @Override
    public Object element() {
        assert this.size > 0 : "Queue is empty";
        return elementImpl();
    }

    @Override
    public boolean isEmpty() {
        return this.size == 0;
    }

    @Override
    public int size() {
        return this.size;
    }

    @Override
    public void clear() {
        this.size = 0;
        clearImpl();
    }
}
