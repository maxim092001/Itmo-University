import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Queue;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
 
/**
 * @author Grankin Maxim (maximgran@gmail.com) at 10:32 23.12.2020
 */
public class F {
 
    private static final List<Edge> edges = new ArrayList<>();
    private static final Set<Integer> mySet = new HashSet<>();
 
    public static void main(String[] args) throws FileNotFoundException {
        InputReader in = new InputReader(new FileInputStream("rainbow.in"));
        PrintStream out = new PrintStream(new FileOutputStream("rainbow.out"));
 
        int n = in.nextInt();
        int m = in.nextInt();
 
        for (int i = 0; i < m; i++) {
            mySet.add(i);
            int x = in.nextInt() - 1;
            int y = in.nextInt() - 1;
            int color = in.nextInt() - 1;
            edges.add(new Edge(x, y, color));
        }
 
        Set<Integer> st = new HashSet<>();
        while (true) {
 
            Set<Integer> first = new HashSet<>();
            Set<Integer> second = new HashSet<>();
 
            List<List<Integer>> graph = createGraphByData(st, first, second, n);
            Set<Integer> path = generatePath(graph, first, second);
            if (path.isEmpty()) {
                break;
            } else {
                st = symmetricDiff(st, path);
            }
        }
        out.println(st.size());
        st.forEach(el -> out.print((el + 1) + " "));
        out.close();
    }
 
    private static List<List<Integer>> createGraphByData(final Set<Integer> set,
                                                         final Set<Integer> first,
                                                         final Set<Integer> second,
                                                         final int n) {
 
        List<List<Integer>> graph = IntStream.range(0, mySet.size()).mapToObj(el -> new ArrayList<Integer>()).collect(Collectors.toList());
        List<Integer> left = new ArrayList<>(set);
        List<Integer> right = mySet.stream().filter(Predicate.not(set::contains)).collect(Collectors.toList());
 
        for (int i = 0; i < left.size(); i++) {
            DSU dsu = new DSU(n);
            boolean flag = false;
 
            for (int j = 0; j < left.size(); j++) {
                if (i == j) {
                    continue;
                }
 
                Edge edge = edges.get(left.get(j));
 
                if (dsu.check(edge.x, edge.y)) {
                    flag = true;
                    break;
                }
                dsu.union(edge.x, edge.y);
            }
 
            if (flag) {
                continue;
            }
 
            final int leftIdx = left.get(i);
            right.stream()
                    .filter(el -> !dsu.check(edges.get(el).x, edges.get(el).y))
                    .forEach(el -> graph.get(leftIdx).add(el));
        }
 
        return f(set, first, second, n, graph, left, right);
    }
 
    private static List<List<Integer>> f(final Set<Integer> set,
                                         final Set<Integer> first,
                                         final Set<Integer> second,
                                         final int n,
                                         final List<List<Integer>> graph,
                                         final List<Integer> left,
                                         final List<Integer> right) {
 
        DSU dsu = new DSU(n);
        boolean flag = false;
 
        for (Integer el : left) {
            if (dsu.check(edges.get(el).x, edges.get(el).y)) {
                flag = true;
            }
            dsu.union(edges.get(el).x, edges.get(el).y);
        }
 
 
        if (!flag) {
            right.stream()
                    .filter(Predicate.not(el -> dsu.check(edges.get(el).x, edges.get(el).y)))
                    .forEach(first::add);
        }
 
        int[] colors = new int[200];
 
        set.forEach(el -> colors[edges.get(el).color]++);
 
        for (Integer rightElement : right) {
            boolean removed = false;
 
            if (colors[edges.get(rightElement).color] > 0) {
                removed = true;
                colors[edges.get(rightElement).color]--;
            }
 
            left.stream()
                    .filter(leftElement -> colors[edges.get(leftElement).color] == 0)
                    .forEach(leftElement -> graph.get(rightElement).add(leftElement));
 
            if (removed) {
                colors[edges.get(rightElement).color]++;
            }
        }
 
        right.stream().filter(el -> colors[edges.get(el).color] == 0).forEach(second::add);
        return graph;
    }
 
    private static Set<Integer> generatePath(final List<List<Integer>> g,
                                             final Set<Integer> first,
                                             final Set<Integer> second) {
        int temp = g.size();
        g.add(new ArrayList<>());
 
        int[] p = new int[g.size()];
        Arrays.fill(p, -1);
        boolean[] used = new boolean[g.size()];
        g.get(g.size() - 1).addAll(first);
 
        Queue<Integer> q = new ArrayDeque<>();
        q.add(temp);
 
        boolean flag = false;
 
        int start = -1;
 
        while (!q.isEmpty() && !flag) {
            int v = q.remove();
 
            for (Integer to : g.get(v)) {
                if (!used[to]) {
                    q.add(to);
                    used[to] = true;
                    p[to] = v;
                }
 
                if (second.contains(to)) {
                    start = to;
                    flag = true;
                    break;
                }
            }
        }
 
        Set<Integer> path = new HashSet<>();
 
        if (!flag) {
            return path;
        }
        while (p[start] != -1) {
            path.add(start);
            start = p[start];
        }
        path.add(start);
        path.remove(temp);
        return path;
    }
 
    private static Set<Integer> symmetricDiff(final Set<Integer> first, final Set<Integer> second) {
        Set<Integer> result = new HashSet<>(first);
        second.stream().filter(Predicate.not(result::add)).forEach(result::remove);
        return result;
    }
 
    private static class Edge {
        int x;
        int y;
        int color;
 
        public Edge(final int x, final int y, final int color) {
            this.x = x;
            this.y = y;
            this.color = color;
        }
    }
 
    private static class DSU {
        int[] p;
        int[] rank;
 
        public DSU(final int n) {
            p = IntStream.range(0, n).toArray();
            rank = new int[n];
            Arrays.fill(rank, 1);
        }
 
        public int get(final int v) {
            if (v == p[v]) {
                return v;
            }
            return p[v] = get(p[v]);
        }
 
        public void union(int v, int u) {
            v = get(v);
            u = get(u);
 
            if (v != u) {
                if (rank[v] > rank[u]) {
                    int temp = v;
                    v = u;
                    u = temp;
                }
                p[v] = u;
                rank[u] += rank[v];
            }
        }
 
        public boolean check(int v, int u) {
            v = get(v);
            u = get(u);
            return u == v;
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