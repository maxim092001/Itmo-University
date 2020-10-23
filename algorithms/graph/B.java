import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;

/**
 * @author Grankin Maxim (maximgran@gmail.com) at 20:31 11.10.2020
 */
public class B {

    private static final List<List<Integer>> graph = new ArrayList<>();
    private static final Map<Edge, Boolean> isBridge = new HashMap<>();
    private static boolean[] used;
    private static int[] timeIn;
    private static int[] timeOut;
    private static int timer;
    private static int bridges;

    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);

        int n = in.nextInt();
        int m = in.nextInt();

        used = new boolean[n];
        timeIn = new int[n];
        timeOut = new int[n];

        List<Edge> edges = new ArrayList<>();

        for (int i = 0; i < n; i++) {
            graph.add(new ArrayList<>());
        }

        for (int i = 0; i < m; i++) {
            int x = in.nextInt();
            int y = in.nextInt();
            graph.get(x - 1).add(y - 1);
            graph.get(y - 1).add(x - 1);
            edges.add(new Edge(x - 1, y - 1));
            isBridge.put(new Edge(x - 1, y - 1), false);
        }

        for (int i = 0; i < n; i++) {
            if (!used[i]) {
                findBridges(i, -1);
            }
        }
        System.out.println(bridges);
        for (int i = 0; i < edges.size(); i++) {
            if (isBridge.get(edges.get(i))) {
                System.out.println(i + 1);
            }
        }
    }

    private static void findBridges(int v, int p) {
        used[v] = true;
        timer++;
        timeIn[v] = timeOut[v] = timer;
        for (Integer to : graph.get(v)) {
            if (to == p) {
                continue;
            }
            if (used[to]) {
                timeOut[v] = Math.min(timeOut[v], timeIn[to]);
            } else {
                findBridges(to, v);
                timeOut[v] = Math.min(timeOut[v], timeOut[to]);
                if (timeOut[to] > timeIn[v]) {
                    isBridge.put(new Edge(v, to), true);
                    isBridge.put(new Edge(to, v), true);
                    bridges++;
                }
            }
        }
    }


    static class Edge {
        int v;
        int u;

        public Edge() {
        }

        public Edge(final int v, final int u) {
            this.v = v;
            this.u = u;
        }

        @Override
        public String toString() {
            return v + " " + u;
        }

        @Override
        public boolean equals(final Object o) {
            if (this == o) return true;
            if (!(o instanceof Edge)) return false;

            Edge edge = (Edge) o;

            if (v != edge.v) return false;
            return u == edge.u;
        }

        @Override
        public int hashCode() {
            int result = v;
            result = 31 * result + u;
            return result;
        }
    }
}
