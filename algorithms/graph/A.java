import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

/**
 * @author Grankin Maxim (maximgran@gmail.com) at 20:31 11.10.2020
 */
public class A {

    private static final List<List<Integer>> graph = new ArrayList<>();
    private static final List<Integer> mark = new ArrayList<>();
    private static final List<Boolean> used = new ArrayList<>();

    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);

        int n = in.nextInt();
        int m = in.nextInt();

        for (int i = 0; i < n; i++) {
            graph.add(new ArrayList<>());
            mark.add(0);
            used.add(false);
        }

        for (int i = 0; i < m; i++) {
            int x = in.nextInt();
            int y = in.nextInt();
            graph.get(x - 1).add(y - 1);
        }

        for (int i = 0; i < n; i++) {
            if (mark.get(i) == 0) {
                if (hasCycle(i)) {
                    System.out.println(-1);
                    System.exit(0);
                }
            }
        }

        List<Integer> ans = new ArrayList<>();

        for (int i = 0; i < n; i++) {
            if (!used.get(i)) {
                ans.addAll(dfs(i));
            }
        }

        for (int i = ans.size() - 1; i >= 0; i--) {
            System.out.print((ans.get(i) + 1) + " ");
        }
    }

    private static List<Integer> dfs(final int v) {
        used.set(v, true);
        List<Integer> ans = new ArrayList<>();
        for (Integer to : graph.get(v)) {
            if (!used.get(to)) {
                ans.addAll(dfs(to));
            }
        }
        ans.add(v);
        return ans;
    }

    private static boolean hasCycle(final int v) {
        mark.set(v, 1);
        for (Integer to : graph.get(v)) {
            if (mark.get(to) == 0) {
                if (hasCycle(to)) {
                    return true;
                }
            } else if (mark.get(to) == 1) {
                return true;
            }
        }
        mark.set(v, 2);
        return false;
    }

}
