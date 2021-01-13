import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Queue;
import java.util.Scanner;
import java.util.StringTokenizer;
import java.util.TreeSet;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
 
public class G {
 
 
    public static void main(String[] args) {
        InputReader in = new InputReader(System.in);
        PrintWriter out = new PrintWriter(System.out);
        int n = in.nextInt();
        int m = in.nextInt();
        List<IntList> lst = new ArrayList<>();
        List<TreeSet<Integer>> edges = new ArrayList<>();
 
        boolean[] used = new boolean[n];
        int[] colors = new int[n];
        Arrays.fill(colors, -1);
 
        for (int i = 0; i < n; i++) {
            lst.add(new IntList());
        }
 
        for (int i = 0; i < m; i++) {
            int x = in.nextInt() - 1;
            int y = in.nextInt() - 1;
            lst.get(x).add(y);
            lst.get(y).add(x);
        }
 
 
        int k = 0;
 
        for (int i = 0; i < n; i++) {
            k = Math.max(k, lst.get(i).size());
        }
 
        k += (k % 2 == 0 ? 1 : 0);
 
 
 
        for (int i = 0; i < n; i++) {
            edges.add(IntStream.rangeClosed(1, k).boxed().collect(Collectors.toCollection(
                    () -> new TreeSet<>(Integer::compareTo)
            )));
        }
 
 
        Queue<Integer> q = new ArrayDeque<>();
        q.add(0);
 
        while (!q.isEmpty()) {
            int t = q.element();
            if (colors[t] == -1) {
                colors[t] = edges.get(t).first();
                for (int i = 0; i < lst.get(t).size(); i++) {
                    edges.get(lst.get(t).get(i)).remove(colors[t]);
                }
            }
            used[t] = true;
            for (int i = 0; i < lst.get(t).size(); i++) {
                int to = lst.get(t).get(i);
                if (!used[to]) {
                    q.add(to);
                }
            }
            q.poll();
        }
        out.println(k);
        Arrays.stream(colors).forEach(out::println);
        out.close();
    }
 
    static class InputReader {
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