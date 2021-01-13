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
import java.util.List;
import java.util.StringTokenizer;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
 
/**
 * @author Grankin Maxim (maximgran@gmail.com) at 10:32 23.12.2020
 */
public class C {
 
 
    private static boolean[] used;
    private static List<List<Integer>> graph;
    private static int[] k;
 
    private static boolean kuhn(int v) {
        if (used[v]) {
            return false;
        }
        used[v] = true;
        for (Integer node : graph.get(v)) {
            if (k[node] == -1 || kuhn(k[node])) {
                k[node] = v;
                return true;
            }
        }
        return false;
    }
 
    public static void main(String[] args) throws FileNotFoundException {
        InputReader in = new InputReader(new FileInputStream("matching.in"));
        PrintStream out = new PrintStream(new FileOutputStream("matching.out"));
 
        int n = in.nextInt();
 
        graph = IntStream.range(0, n).mapToObj(i -> new ArrayList<Integer>()).collect(Collectors.toList());
        k = IntStream.range(0, n).map(i -> -1).toArray();
 
        List<WeightAndIndex> weightAndIndices = new ArrayList<>();
 
        for (int i = 0; i < n; i++) {
            weightAndIndices.add(new WeightAndIndex(in.nextInt(), i));
        }
 
        weightAndIndices.sort(Comparator.comparingInt((WeightAndIndex a) -> a.w).thenComparingInt(a -> a.i));
        Collections.reverse(weightAndIndices);
 
        for (int i = 0; i < n; i++) {
            int k = in.nextInt();
 
            for (int j = 0; j < k; j++) {
                int v = in.nextInt() - 1;
                graph.get(i).add(v);
            }
        }
 
        for (int i = 0; i < n; i++) {
            used = new boolean[n];
            kuhn(weightAndIndices.get(i).i);
        }
 
        int[] ans = IntStream.range(0, n).map(i -> -1).toArray();
        for (int i = 0; i < n; i++) {
            if (k[i] != -1) {
                ans[k[i]] = i;
            }
        }
 
        Arrays.stream(ans).forEach(el -> out.print((el + 1) + " "));
        out.close();
    }
 
    private static class WeightAndIndex {
        int w;
        int i;
 
        public WeightAndIndex(final int w, final int i) {
            this.w = w;
            this.i = i;
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