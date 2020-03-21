package queue;

import base.Asserts;
import base.TestCounter;

import java.io.File;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.Random;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class ArrayQueueTest<T extends ArrayQueueTest.Queue> extends Asserts {
    private final QueueFactory<T> factory;
    private final TestCounter counter = new TestCounter();

    private static final Object[] ELEMENTS = new Object[]{
            "Hello",
            "world",
            1, 2, 3,
            new Object()
    };

    protected final Random random = new Random(2474258720358724587L);

    public ArrayQueueTest(final QueueFactory<T> factory) {
        Asserts.checkAssert(getClass());
        this.factory = factory;
    }

    public static void main(final String[] args) {
        new ArrayQueueTest<>(Queue::new).test();
    }

    protected void test() {
        test("ArrayQueue", 2, Mode.values());
    }

    protected void test(final String className, final int step, final Mode... modes) {
        for (final Mode mode : modes) {
            System.err.printf("Running %s for %s in %s mode%n", getClass().getName(), className, mode);
            test(className, mode, step);
        }
        counter.printStatus(getClass());
    }

    private void test(final String className, final Mode mode, final int step) {
        testEmpty(create(className, mode));
        testSingleton(create(className, mode));
        testClear(create(className, mode));
        for (int i = 0; i <= 10; i += step) {
            testRandom(create(className, mode), 100_000, (double) i / 10);
        }
    }

    protected T create(final String className, final Mode mode) {
        try {
            return factory.create(className, mode);
        } catch (MalformedURLException | NoSuchMethodException | ClassNotFoundException e) {
            throw new AssertionError("Cannot create Queue (" + className + "): " + e.getMessage());
        }
    }

    protected void testEmpty(final T queue) {
        nextTest("testEmpty");
        assertSize(0, queue);
        counter.passed();
    }

    protected void testSingleton(final T queue) {
        nextTest("testSingleton");
        assertSize(0, queue);
        final String value = "value";
        queue.enqueue(value);
        assertSize(1, queue);
        assertEquals("element()", value, queue.element());
        assertEquals("dequeue()", value, queue.dequeue());
        assertSize(0, queue);
        counter.passed();
    }

    private void nextTest(final String name) {
        System.err.println("=== " + name);
        counter.nextTest();
    }

    protected void testClear(final T queue) {
        nextTest("testClear");
        assertSize(0, queue);

        final String value = "value";
        queue.enqueue(value);
        queue.enqueue(value);
        queue.clear();
        assertSize(0, queue);

        final String value1 = "value1";
        queue.enqueue(value1);
        assertEquals("deque()", value1, queue.dequeue());
        counter.passed();
    }

    protected void testRandom(final T queue, final int operations, final double addFreq) {
        nextTest("testRandom, add frequency = " + addFreq);
        final Deque<Object> deque = new ArrayDeque<>();
        int ops = 0;
        for (int i = 0; i < operations; i++) {
            if (deque.isEmpty() || random.nextDouble() < addFreq) {
                add(deque, queue, randomElement());
            } else {
                remove(deque, queue);
            }
            checkAndSize(deque, queue);
            if (ops++ >= deque.size() && random.nextDouble() < 0.25) {
                ops -= deque.size();
                linearTest(deque, queue);
            }
        }
        linearTest(deque, queue);
        while (!deque.isEmpty()) {
            remove(deque, queue);
            checkAndSize(deque, queue);
        }
        counter.passed();
    }

    private void checkAndSize(final Deque<Object> deque, final T queue) {
        if (!deque.isEmpty() && random.nextBoolean()) {
            check(deque, queue);
        }
        assertSize(deque.size(), queue);
    }

    protected void remove(final Deque<Object> deque, final T queue) {
        assertEquals("dequeue()", deque.removeFirst(), queue.dequeue());
    }

    protected void check(final Deque<Object> deque, final T queue) {
        assertEquals("element()", deque.element(), queue.element());
    }

    protected void add(final Deque<Object> deque, final T queue, final Object element) {
        deque.addLast(element);
        queue.enqueue(element);
    }

    protected void linearTest(final Deque<Object> deque, final T queue) {
        // Do nothing by default
    }

    protected Object randomElement() {
        return ELEMENTS[random.nextInt(ELEMENTS.length)];
    }

    protected void assertSize(final int size, final T queue) {
        assertEquals("size()", size, queue.size());
        assert queue.size() == size : "Expected size() " + size + ", found " + queue.size();
        assert (size == 0) == queue.isEmpty() : "Expected isEmpty() " + (size == 0) + ", found " + queue.isEmpty();
    }

    enum Mode {
        MODULE("Module") {
            @Override
            public <T> ZMethod<T> getMethod(final Object instance, final String name, final Class<?>... args) throws NoSuchMethodException {
                return checkStatic(true, new ZMethod<>(instance, name, args));
            }
        },
        ADT("ADT") {
            @Override
            public <T> ZMethod<T> getMethod(final Object instance, final String name, final Class<?>... args) throws NoSuchMethodException {
                final Object[] a = new Object[args.length + 1];
                a[0] = instance;
                final Class<?>[] result = new Class<?>[args.length + 1];
                result[0] = instance.getClass();
                System.arraycopy(args, 0, result, 1, args.length);

                return checkStatic(true, new ZMethod<T>(instance, name, result) {
                    @Override
                    protected T invoke(final Object... args) {
                        if (args.length == 1) {
                            a[1] = args[0];
                        }
                        return super.invoke(a);
                    }
                });
            }
        },
        CLASS("") {
            @Override
            public <T> ZMethod<T> getMethod(final Object instance, final String name, final Class<?>... args) throws NoSuchMethodException {
                return checkStatic(false, new ZMethod<>(instance, name, args));
            }
        };

        private final String suffix;

        Mode(final String suffix) {
            this.suffix = suffix;
        }

        private static <T> ZMethod<T> checkStatic(final boolean isStatic, final ZMethod<T> method) {
            if (isStatic != Modifier.isStatic(method.method.getModifiers())) {
                throw new AssertionError("Method " + method.method.getName() + " in " + method.method.getDeclaringClass() + (isStatic ? " must" : " must not") + " be static");
            }
            return method;
        }

        public abstract <T> ZMethod<T> getMethod(Object instance, String name, Class<?>... args) throws NoSuchMethodException;
    }

    protected static class ZMethod<T> {
        private final Object instance;
        private final Method method;

        public ZMethod(final Object instance, final String name, final Class<?>... args) throws NoSuchMethodException {
            this.instance = instance;
            method = instance.getClass().getMethod(name, args);
        }

        protected T invoke(final Object... args) {
            try {
                @SuppressWarnings("unchecked")
                final T result = (T) method.invoke(instance, args);
                return result;
            } catch (final Exception e) {
                throw new AssertionError("Error calling method " + method.getName(), e);
            }
        }
    }

    public interface QueueFactory<T> {
        T create(String className, Mode mode) throws MalformedURLException, NoSuchMethodException, ClassNotFoundException;
    }

    static class Queue {
        private final ZMethod<Void> enqueue;
        private final ZMethod<Object> element;
        private final ZMethod<Object> dequeue;
        private final ZMethod<Integer> size;
        private final ZMethod<Boolean> isEmpty;
        private final ZMethod<Void> clear;
        private final Mode mode;
        protected final Object instance;

        public Queue(final String name, final Mode mode) throws MalformedURLException, NoSuchMethodException, ClassNotFoundException {
            this(createInstance(name, mode), mode);
        }

        public Queue(final Object instance, final Mode mode) throws NoSuchMethodException {
            this.mode = mode;
            this.instance = instance;

            enqueue = findMethod("enqueue", Object.class);
            element = findMethod("element");
            dequeue = findMethod("dequeue");
            size = findMethod("size");
            isEmpty = findMethod("isEmpty");
            clear = findMethod("clear");
        }

        private static Object createInstance(final String name, final Mode mode) throws MalformedURLException, ClassNotFoundException {
            final String className = "queue." + name + mode.suffix;
            final URL url = new File(".").toURI().toURL();
            final Class<?> clazz = new URLClassLoader(new URL[]{url}).loadClass(className);
            try {
                return clazz.newInstance();
            } catch (final Exception e) {
                throw new AssertionError("Cannot create instance of " + className, e);
            }
        }

        protected <T> ZMethod<T> findMethod(final String name, final Class<?>... args) throws NoSuchMethodException {
            return mode.getMethod(instance, name, args);
        }

        public void enqueue(final Object element) {
            enqueue.invoke(element);
        }

        public Object element() {
            return element.invoke();
        }

        public Object dequeue() {
            return dequeue.invoke();
        }

        public int size() {
            return size.invoke();
        }

        public boolean isEmpty() {
            return isEmpty.invoke();
        }

        public void clear() {
            clear.invoke();
        }
    }
}
