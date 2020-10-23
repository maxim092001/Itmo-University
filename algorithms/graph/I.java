import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

/**
 * @author Grankin Maxim (maximgran@gmail.com) at 22:05 21.10.2020
 */
public class I {

    private static final int N = 200_010;
    private static final double[] medge = new double[N];
    private static final int[] e = new int[N];
    private static final boolean[] used = new boolean[N];
    private static final Pair[] vectors = new Pair[N];

    private static double sqr(final double x) {
        return x * x;
    }
    private static double distance(final Pair x, final Pair y) {
        return Math.sqrt(sqr(x.x - y.x) + sqr(x.y - y.y));
    }
    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);

        int n = in.nextInt();

        for (int i = 0; i < n; i++) {
            int x = in.nextInt();
            int y = in.nextInt();
            vectors[i] = new Pair(x, y);
        }

        for (int i = 0; i < n; i++) {
            medge[i] = Double.MAX_VALUE;
            e[i] = -1;
        }

        medge[0] = 0;

        double ans = 0;
        for (int i= 0 ; i < n; i++) {
            int u = -1;

            for (int j = 0; j < n; j++) {
                if (!used[j] && (u == -1 || medge[j] < medge[u])) {
                    u = j;
                }
            }

            used[u] = true;

            if (e[u] != -1) {
                ans += distance(vectors[u], vectors[e[u]]);
            }

            for (int j = 0; j < n; j++) {
                if (distance(vectors[u], vectors[j]) < medge[j]) {
                    medge[j] = distance(vectors[u], vectors[j]);
                    e[j] = u;
                }
            }
        }
        System.out.println(ans);
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
