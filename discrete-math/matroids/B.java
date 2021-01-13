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
import java.util.Comparator;
import java.util.List;
import java.util.StringTokenizer;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
 
/**
 * @author Grankin Maxim (maximgran@gmail.com) at 10:32 23.12.2020
 */
public class B {
 
    private static List<Integer> p;
    private static final List<Edge> edges = new ArrayList<>();
 
    private static int dsuGet(int v) {
        if (v == p.get(v)) {
            return v;
        }
        p.set(v, dsuGet(p.get(v)));
        return p.get(v);
    }
 
    private static void dsuUnion(int v, int u) {
        v = dsuGet(v);
        u = dsuGet(u);
 
        if (v == u) {
            return;
        }
        p.set(v, u);
    }
 
    private static boolean dsuCheck(int v, int u) {
        v = dsuGet(v);
        u = dsuGet(u);
        return u == v;
    }
 
    public static void main(String[] args) throws FileNotFoundException {
        InputReader in = new InputReader(new FileInputStream("destroy.in"));
        PrintStream out = new PrintStream(new FileOutputStream("destroy.out"));
 
        int n = in.nextInt();
        int m = in.nextInt();
        long s = in.nextLong();
 
        p = IntStream.range(0, n).boxed().collect(Collectors.toList());
 
        for (int i = 0; i < m; i++) {
            int x = in.nextInt() - 1;
            int y = in.nextInt() - 1;
            long cost = in.nextLong();
 
            edges.add(new Edge(cost, i, x, y));
        }
        edges.sort(Comparator.comparingLong((Edge edge) -> edge.cost).reversed().thenComparingInt(edge -> edge.index));
 
        List<Edge> newEdges = new ArrayList<>();
 
        for (Edge edge : edges) {
            if (!dsuCheck(edge.x, edge.y)) {
                dsuUnion(edge.x, edge.y);
            } else {
                newEdges.add(edge);
            }
        }
 
        newEdges.sort(Comparator.comparingLong((Edge edge) -> edge.cost).thenComparingInt(edge -> edge.index));
 
        List<Integer> result = new ArrayList<>();
 
        long currentW = 0;
 
        for (Edge edge : newEdges) {
            if (currentW + edge.cost <= s) {
                result.add(edge.index);
                currentW += edge.cost;
            }
        }
 
        out.println(result.size());
        result.stream().sorted().forEach(el -> out.print((el + 1) + " "));
        out.close();
    }
 
    private static class Edge {
        long cost;
        int index;
        int x;
        int y;
 
        public Edge(final long cost, final int index, final int x, final int y) {
            this.cost = cost;
            this.index = index;
            this.x = x;
            this.y = y;
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