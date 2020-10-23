import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.Scanner;

/**
 * @author Grankin Maxim (maximgran@gmail.com) at 09:52 22.10.2020
 */
public class J {

    private static final int N = 200_010;
    private static final List<List<Pair<Integer, Integer>>> graph = new ArrayList<>();
    private static final List<Pair<Integer, Pair<Integer, Integer>>> edges = new ArrayList<>();

    private static final int[] p = new int[N];
    private static final int[] r = new int[N];

    private static int dsuGet(int v) {
        if (v == p[v]) {
            return v;
        }
        return p[v] = dsuGet(p[v]);
    }

    private static void dsuUnion(int v, int u) {
        v = dsuGet(v);
        u = dsuGet(u);

        if (v != u) {
            if (r[v] > r[u]) {
                int temp = r[v];
                r[v] = r[u];
                r[u] = temp;
            }

            p[v] = u;
            r[u] += r[v];
        }
    }

    private static boolean dsuCheck(int v, int u) {
        v = dsuGet(v);
        u = dsuGet(u);
        return v == u;
    }
    public static void main(String[] args) {
//        System.out.println(new Pair<>(1, 2).compareTo(new Pair<>(2, 3)));
        Scanner in = new Scanner(System.in);
        int n = in.nextInt();
        int m = in.nextInt();

        for (int i = 0; i < n; i++) {
            graph.add(new ArrayList<>());
        }

        for (int i = 0; i < m; i++) {
            int x = in.nextInt() - 1;
            int y = in.nextInt() - 1;
            int z = in.nextInt();

            graph.get(x).add(new Pair<>(y, z));
            graph.get(x).add(new Pair<>(x, z));
            edges.add(new Pair<>(z, new Pair<>(x, y)));
        }

        for (int i = 0; i < n; i++) {
            p[i] = i;
            r[i] = 1;
        }


        edges.sort(Pair::compareTo);
        long weight = 0;

        for (int i = 0; i < m; i++) {
            var e = edges.get(i).second;
            if (!dsuCheck(e.first, e.second)) {
                dsuUnion(e.first, e.second);
                weight += edges.get(i).first;
            }
        }

        System.out.println(weight);
    }

    private static class Pair<K extends Comparable<K>, V extends Comparable<V>> implements Comparable<Pair<K, V>> {
        K first;
        V second;

        public Pair(final K first, final V second) {
            this.first = first;
            this.second = second;
        }

        @Override
        public boolean equals(final Object o) {
            if (this == o) return true;
            if (!(o instanceof Pair)) return false;

            Pair<?, ?> pair = (Pair<?, ?>) o;

            if (!Objects.equals(first, pair.first)) return false;
            return Objects.equals(second, pair.second);
        }

        @Override
        public int hashCode() {
            int result = first != null ? first.hashCode() : 0;
            result = 31 * result + (second != null ? second.hashCode() : 0);
            return result;
        }

        @Override
        public int compareTo(final Pair o) {
            return new DefaultComparator().compare(this, o);
        }

        private class DefaultComparator implements Comparator<Pair<K, V>> {

            @Override
            public int compare(final Pair<K, V> o1, final Pair<K, V> o2) {
                int first = o1.first.compareTo(o2.first);
                if (first != 0) {
                    return first;
                }
                return o1.second.compareTo(o2.second);
            }
        }
    }
}
