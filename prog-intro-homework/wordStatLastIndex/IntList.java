import java.util.Arrays;
import java.util.Iterator;
import java.util.NoSuchElementException;

public class IntList implements Iterable<Integer> {

    private int array[];
    private int size;

    public IntList() {
        init();
    }

    public IntList(int a) {
        init();
        add(a);
    }

    private void init() {
        array = new int[8];
        size = 0;
    }

    public void add(int value) {
        addToList(value);
    }

    private void addToList(int value) {
        if (size == array.length) {
            array = Arrays.copyOf(array, array.length + (array.length >> 1));
        }
        array[size++] = value;
    }

    public int size() {
        return this.size;
    }

    public int get(int ind) {
        return array[ind];
    }

    public int back() {
        if (size == 0) {
            throw new NoSuchElementException();
        }
        return array[size - 1];
    }

    @Override
    public Iterator<Integer> iterator() {
        return new MyIterator();
    }

    private class MyIterator implements Iterator<Integer> {

        private int index = 0;

        @Override
        public boolean hasNext() {
            return index < size();
        }

        @Override
        public Integer next() {
            return get(index++);
        }
    }
}
