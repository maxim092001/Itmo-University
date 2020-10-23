import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Scanner;
import java.util.Set;

/**
 * @author Grankin Maxim (maximgran@gmail.com) at 21:46 21.10.2020
 */
public class F {


    private static final List<List<Integer>> graph = new ArrayList<>();
    private static final List<List<Integer>> invertedGraph = new ArrayList<>();
    private static final Set<Pair> isConnected = new HashSet<>();

    private static boolean[] used;
    private static int[] comp;
    private static int color = 1;

    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        int n = in.nextInt();
        int m = in.nextInt();
        used = new boolean[n];
        comp = new int[n];

        for (int i = 0; i < n; i++) {
            graph.add(new ArrayList<>());
            invertedGraph.add(new ArrayList<>());
        }

        for (int i = 0; i < m; i++) {
            int x = in.nextInt() - 1;
            int y = in.nextInt() - 1;

            graph.get(x).add(y);
            invertedGraph.get(y).add(x);
        }

        List<Integer> lst = new ArrayList<>();

        for (int i = 0; i < n; i++) {
            if (!used[i]) {
                lst.addAll(dfs1(i));
            }
        }

        for (int i = lst.size() - 1; i >= 0; i--) {
            if (comp[lst.get(i)] == 0) {
                dfs2(lst.get(i));
                color++;
            }
        }

        int ans = 0;

        for (int i = 0; i < n; i++) {
            for (int j = 0; j < graph.get(i).size(); j++) {
                Pair pair = new Pair(
                        Math.min(comp[i], comp[graph.get(i).get(j)]),
                        Math.max(comp[i], comp[graph.get(i).get(j)]));

                if (comp[i] != comp[graph.get(i).get(j)] && !isConnected.contains(pair)) {
                    isConnected.add(pair);
                    ans++;
                }
            }
        }

        System.out.println(ans);
    }

    private static List<Integer> dfs1(int v) {
        used[v] = true;
        List<Integer> ans = new ArrayList<>();

        for (Integer to : graph.get(v)) {
            if (!used[to]) {
                ans.addAll(dfs1(to));
            }
        }
        ans.add(v);
        return ans;
    }

    private static void dfs2(int v) {
        comp[v] = color;

        for (Integer to : invertedGraph.get(v)) {
            if (comp[to] == 0) {
                dfs2(to);
            }
        }
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
