import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.InputMismatchException;
import java.util.List;
import java.util.Scanner;

/**
 * @author Grankin Maxim (maximgran@gmail.com) at 10:32 22.10.2020
 */
public class H {

    private static final List<List<Pair>> graph = new ArrayList<>();
    private static final List<List<Pair>> invertedGraph = new ArrayList<>();
    private static int color;
    private static int[] used;
    private static final int INF = 1_000_000_100;

    public static void main(String[] args) throws IOException {
        var in = new FastScanner(new FileInputStream("avia.in"));
        int n = in.nextInt();

        used = new int[n];

        for (int i = 0; i < n; i++) {
            graph.add(new ArrayList<>());
            invertedGraph.add(new ArrayList<>());
        }

        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                int x = in.nextInt();
                graph.get(i).add(new Pair(j, x));
                invertedGraph.get(j).add(new Pair(i, x));
            }
        }

        in.close();
        int left = -1, right = INF;

        while (right - left > 1) {
            int mid = (right + left) / 2;
            if (check(mid)) {
                right = mid;
            } else {
                left = mid;
            }
        }
        PrintWriter printWriter = new PrintWriter(new FileWriter("avia.out"));
        printWriter.println(right);
        printWriter.close();
    }


    private static void dfs(int v, int p, int m, final List<List<Pair>> g) {
        used[v] = color;

        for (var to : g.get(v)) {
            if (to.first != p && used[to.first] == 0 && to.second <= m) {
                dfs(to.first, v, m, g);
            }
        }
    }

    private static boolean check(int n) {
        color = 0;
        Arrays.fill(used, 0);

        for (int i = 0; i < used.length; i++) {
            if (used[i] == 0) {
                color++;
                dfs(i, -1, n, graph);
            }
        }

        int temp = color;
        color = 0;

        Arrays.fill(used, 0);

        for (int i = 0; i < used.length; i++) {
            if (used[i] == 0) {
                color++;
                dfs(i, -1, n, invertedGraph);
            }
        }

        return color == 1 && temp == 1;
    }

    private static class Pair{
        int first;
        int second;

        public Pair(final int first, final int second) {
            this.first = first;
            this.second = second;
        }
    }

    static class FastScanner extends BufferedReader {
        public FastScanner(InputStream is) {
            super(new InputStreamReader(is));
        }

        public int read() {
            try {
                int ret = super.read();
//            if (isEOF && ret < 0) {
//                throw new InputMismatchException();
//            }
//            isEOF = ret == -1;
                return ret;
            } catch (IOException e) {
                throw new InputMismatchException();
            }
        }

        static boolean isWhiteSpace(int c) {
            return c >= 0 && c <= 32;
        }

        public int nextInt() {
            int c = read();
            while (isWhiteSpace(c)) {
                c = read();
            }
            int sgn = 1;
            if (c == '-') {
                sgn = -1;
                c = read();
            }
            int ret = 0;
            while (c >= 0 && !isWhiteSpace(c)) {
                if (c < '0' || c > '9') {
                    throw new NumberFormatException("digit expected " + (char) c
                            + " found");
                }
                ret = ret * 10 + c - '0';
                c = read();
            }
            return ret * sgn;
        }

        public String readLine() {
            try {
                return super.readLine();
            } catch (IOException e) {
                return null;
            }
        }

    }
}
