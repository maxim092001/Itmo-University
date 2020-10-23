import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

/**
 * @author Grankin Maxim (maximgran@gmail.com) at 21:30 17.10.2020
 */
public class C {

    private static final List<ArrayList<Integer>> graph = new ArrayList<>();
    private static boolean[] isCutPoint;
    private static boolean[] used;
    private static int[] timeIn;
    private static int[] timeOut;
    private static int timer;

    public static void main(String[] args) {

        Scanner in = new Scanner(System.in);
        int n = in.nextInt();
        int m = in.nextInt();

        used = new boolean[n];
        isCutPoint = new boolean[n];
        timeIn = new int[n];
        timeOut = new int[n];

        for (int i = 0; i < n; i++) {
            graph.add(new ArrayList<>());
        }

        for (int i = 0; i < m; i++) {
            int x = in.nextInt();
            int y = in.nextInt();
            graph.get(x - 1).add(y - 1);
            graph.get(y - 1).add(x - 1);
        }

        for (int i = 0; i < n; i++) {
            if (!used[i]) {
                dfs(i, -1);
            }
        }


        int ans = 0;

        for (int i = 0; i < n; i++) {
            ans += isCutPoint[i] ? 1 : 0;
        }

        System.out.println(ans);

        for (int i = 0; i < n; i++) {
            if (isCutPoint[i]) {
                System.out.print((i + 1) + " ");
            }
        }
    }


    private static void dfs(int v, int p) {
        used[v] = true;
        timeIn[v] = timeOut[v] = timer++;
        int children = 0;
        for (Integer to : graph.get(v)) {
            if (to == p) {
                continue;
            }

            if (used[to]) {
                timeOut[v] = Math.min(timeOut[v], timeIn[to]);
            } else {
                dfs(to, v);
                timeOut[v] = Math.min(timeOut[v], timeOut[to]);
                if (timeOut[to] >= timeIn[v] && p != -1) {
                    isCutPoint[v] = true;
                }
                children++;
            }
        }
        if (p == -1 && children > 1) {
            isCutPoint[v] = true;
        }
    }
}
