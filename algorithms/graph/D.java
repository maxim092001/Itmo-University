import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;

/**
 * @author Grankin Maxim (maximgran@gmail.com) at 20:31 11.10.2020
 */
public class D {

    private static List<List<Edge>> graph = new ArrayList<>();
    private static boolean[] used;
    private static boolean[] isBridge;
    private static int[] timeIn;
    private static int[] timeOut;
    private static int[] colors;
    private static int timer;
    private static int color;

    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);

        int n = in.nextInt();
        int m = in.nextInt();

        used = new boolean[n];
        isBridge = new boolean[m];
        timeIn = new int[n];
        timeOut = new int[n];
        colors = new int[n];


        for (int i = 0; i < n; i++) {
            graph.add(new ArrayList<>());
        }

        for (int i = 0; i < m; i++) {
            int x = in.nextInt();
            int y = in.nextInt();
            graph.get(x - 1).add(new Edge(y - 1, i));
            graph.get(y - 1).add(new Edge(x - 1, i));
        }

        for (int i = 0; i < n; i++) {
            if (!used[i]) {
                findBridges(i, -1);
            }
        }

        for (int i = 0; i < n; i++) {
            used[i] = false;
        }

        List<List<Edge>> graph1 = new ArrayList<>();
        for (int i = 0; i < n; i++) {
            graph1.add(new ArrayList<>());
        }

        for (int i = 0; i < n; i++) {
            for (Edge edge : graph.get(i)) {
                if (!isBridge[edge.idx]) {
                    graph1.get(i).add(new Edge(edge.v, edge.idx));
                    graph1.get(edge.v).add(new Edge(i, edge.idx));
                }
            }
        }

        graph = graph1;

        for (int i = 0; i < n; i++) {
            if (!used[i]) {
                color++;
                dfs(i);
            }
        }

        System.out.println(color);
        for (int i = 0; i < n; i++) {
            System.out.print(colors[i] + " ");
        }
    }

    private static void dfs(int v) {
        used[v] = true;
        colors[v] = color;

        for (Edge to : graph.get(v)) {
            if (!used[to.v]) {
                dfs(to.v);
            }
        }
    }

    private static void findBridges(int v, int p) {
        used[v] = true;
        timer++;
        timeIn[v] = timeOut[v] = timer;
        for (Edge t : graph.get(v)) {
            int to = t.v;
            if (to == p) {
                continue;
            }
            if (used[to]) {
                timeOut[v] = Math.min(timeOut[v], timeIn[to]);
            } else {
                findBridges(to, v);
                timeOut[v] = Math.min(timeOut[v], timeOut[to]);
                if (timeOut[to] > timeIn[v]) {
                    isBridge[t.idx] = true;
                }
            }
        }
    }

    static class Edge {
        int v;
        int idx;

        public Edge(final int v, final int idx) {
            this.v = v;
            this.idx = idx;
        }

        @Override
        public String toString() {
            return v + " " + idx;
        }

        @Override
        public boolean equals(final Object o) {
            if (this == o) return true;
            if (!(o instanceof Edge)) return false;

            Edge edge = (Edge) o;

            if (v != edge.v) return false;
            return idx == edge.idx;
        }

        @Override
        public int hashCode() {
            int result = v;
            result = 31 * result + idx;
            return result;
        }
    }
}
