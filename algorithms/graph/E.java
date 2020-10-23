import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;

/**
 * @author Grankin Maxim (maximgran@gmail.com) at 21:18 21.10.2020
 */
public class E {

    private static final Deque<Integer> stack = new ArrayDeque<>();
    private static final Map<Pair, Integer> rEdge = new HashMap<>();
    private static boolean[] used;
    private static int[] tin;
    private static int[] edgeColors;
    private static int t;
    private static int maxColor;
    private static final List<List<Pair>> graph = new ArrayList<>();

    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        int n = in.nextInt();
        tin = new int[n];

        for (int i = 0; i < n; i++) {
            graph.add(new ArrayList<>());
        }

        int m = in.nextInt();
        used = new boolean[m];
        edgeColors = new int[m];
        int[] d = new int[m];


        for (int i = 0; i < m; i++) {
            int u = in.nextInt() - 1;
            int v = in.nextInt() - 1;
            if (u > v) {
                int temp = u;
                u = v;
                v = temp;
            }

            var edge = new Pair(u ,v);
            if (rEdge.containsKey(edge)) {
                d[i] = rEdge.get(edge);
            } else {
                graph.get(u).add(new Pair(v, i));
                graph.get(v).add(new Pair(u, i));
                rEdge.put(edge, i);
            }
        }

        for (int i = 0; i < n; i++) {
            if (tin[i] == 0) {
                dfs(i, -1);
            }
        }

        System.out.println(maxColor);

        for (int i = 0; i < edgeColors.length; i++) {
            if (edgeColors[i] == 0) {
                System.out.print(edgeColors[d[i]] + " ");
            } else {
                System.out.print(edgeColors[i] + " ");
            }
        }
    }

    private static int dfs(int v, int p) {
        tin[v] = ++t;
        int mt = tin[v];

        for (Pair t : graph.get(v)) {
            int to = t.x;
            if (to != p) {
                int time;

                int sz = stack.size();

                if (!used[t.y]) {
                    stack.addFirst(t.y);
                    used[t.y] = true;
                }

                if (tin[to] == 0) {
                    time = dfs(to, v);
                    if (time >= tin[v]) {
                        maxColor++;

                        while (stack.size() != sz) {
                            edgeColors[stack.getFirst()] = maxColor;
                            stack.pollFirst();
                        }
                    }
                } else {
                    time = tin[to];
                }
                mt = Math.min(mt, time);
            }
        }
        return mt;
    }


    static class Pair {
        int x;
        int y;

        public Pair(final int x, final int y) {
            this.x = x;
            this.y = y;
        }

        @Override
        public String toString() {
            return x + " " + y;
        }

        @Override
        public boolean equals(final Object o) {
            if (this == o) return true;
            if (!(o instanceof Pair)) return false;

            Pair pair = (Pair) o;

            if (x != pair.x) return false;
            return y == pair.y;
        }

        @Override
        public int hashCode() {
            int result = x;
            result = 31 * result + y;
            return result;
        }
    }
}
