import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.TreeSet;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
 
/**
 * @author Grankin Maxim (maximgran@gmail.com) at 10:32 23.12.2020
 */
public class E {
 
 
    public static void main(String[] args) throws FileNotFoundException {
        InputReader in = new InputReader(new FileInputStream("cycles.in"));
        PrintStream out = new PrintStream(new FileOutputStream("cycles.out"));
 
        int n = in.nextInt();
        int m = in.nextInt();
 
        Set<List<Integer>> mySet = new HashSet<>();
        List<WeightAndIndex> lst = new ArrayList<>();
 
        for (int i = 1; i <= n; i++) {
            int w = in.nextInt();
            lst.add(new WeightAndIndex(w, i));
        }
 
        lst.sort(Comparator.comparingInt((WeightAndIndex el) -> el.weight).thenComparingInt(el -> el.index).reversed());
 
        for (int i = 0; i < m; i++) {
            mySet.add(IntStream.range(0, in.nextInt())
                    .mapToObj(el -> in.nextInt())
                    .sorted()
                    .collect(Collectors.toList()));
        }
 
        int ans = 0;
        TreeSet<Integer> indexes = new TreeSet<>();
 
        for (WeightAndIndex weightAndIndex : lst) {
            indexes.add(weightAndIndex.index);
 
            boolean flag = false;
            for (int j = 0; j < (1 << indexes.size()); j++) {
 
                int jFinal = j;
 
                flag = mySet.contains(indexes.stream()
                        .filter(el -> ((jFinal >> indexes.headSet(el).size()) & 1) == 1)
                        .collect(Collectors.toList()));
                if (flag) {
                    break;
                }
            }
            if (flag) {
                indexes.remove(weightAndIndex.index);
            } else {
                ans += weightAndIndex.weight;
            }
        }
        out.println(ans);
        out.close();
    }
 
    private static class WeightAndIndex {
        int weight;
        int index;
 
        public WeightAndIndex(final int weight, final int index) {
            this.weight = weight;
            this.index = index;
        }
    }
 
    private static class InputReader {
        public BufferedReader reader;
        public StringTokenizer tokenizer;
 
        public InputReader(InputStream stream) {
            reader = new BufferedReader(new InputStreamReader(stream), 32768);
            tokenizer = null;
        }
 
        public String next() {
            while (tokenizer == null || !tokenizer.hasMoreTokens()) {
                try {
                    tokenizer = new StringTokenizer(reader.readLine());
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
            }
            return tokenizer.nextToken();
        }
 
        public int nextInt() {
            return Integer.parseInt(next());
        }
 
        public long nextLong() {
            return Long.parseLong(next());
        }
 
    }
 
    private static class IntList {
 
        private int[] array;
        private int size;
 
        public IntList() {
            init();
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
 
    }
}