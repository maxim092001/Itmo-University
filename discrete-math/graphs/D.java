import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Scanner;
import java.util.stream.Collectors;
 
/**
 * @author Grankin Maxim (maximgran@gmail.com) at 15:52 13.12.2020
 */
public class D {
 
 
    private static final List<IntList> g = new ArrayList<>();
    private static boolean[] used;
 
    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        int n = in.nextInt();
        in.nextLine();
 
        for (int i = 0; i < n; i++) {
            g.add(new IntList());
        }
 
        for (int i = 0; i < n; i++) {
            String s = in.nextLine();
 
            for (int j = 0; j < i; j++) {
                if (s.charAt(j) == '1') {
                    g.get(i).add(j);
                } else {
                    g.get(j).add(i);
                }
            }
        }
 
        final List<Integer> sorted = new ArrayList<>();
 
        for (int i = 0; i < n; i++) {
            used = new boolean[n];
            dfs(i, sorted);
            Collections.reverse(sorted);
            if (g.get(sorted.get(sorted.size() - 1)).contains(sorted.get(0))) {
                System.out.println(sorted.stream().map(element -> (element + 1) + " ").collect(Collectors.joining()));
                break;
            }
            sorted.clear();
        }
    }
 
    private static void dfs(final int v, final List<Integer> sorted) {
        used[v] = true;
        for (Integer u : g.get(v)) {
            if (!used[u]) {
                dfs(u, sorted);
            }
        }
        sorted.add(v);
    }
}
 
class IntList implements Iterable<Integer> {
 
    private int[] array;
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
 
    public boolean contains(int x) {
        return indexOf(x) >= 0;
    }
 
    public int indexOf(int o) {
        return indexOfRange(o, 0, size);
    }
 
    private int indexOfRange(int o, int start, int end) {
        int[] es = array;
        for (int i = start; i < end; i++) {
            if (o == es[i]) {
                return i;
            }
        }
        return -1;
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