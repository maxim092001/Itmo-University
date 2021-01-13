import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;
import java.util.TreeSet;
 
/**
 * @author Grankin Maxim (maximgran@gmail.com) at 22:34 04.12.2020
 */
public class E {
 
    private static final List<List<Integer>> g = new ArrayList<>();
    private static final TreeSet<Integer> leaves = new TreeSet<>();
 
    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
 
        int n = in.nextInt();
        int[] pow = new int[n];
        boolean[] used = new boolean[n];
 
        for (int i = 0; i < n; i++) {
            g.add(new ArrayList<>());
        }
 
        for (int i = 0; i < n - 1; i++) {
            int x = in.nextInt() - 1;
            int y = in.nextInt() - 1;
            g.get(x).add(y);
            g.get(y).add(x);
            pow[x]++;
            pow[y]++;
        }
 
        int[] ans = new int[n - 2];
 
        for (int i = 0; i < n; i++) {
            if (pow[i] == 1) {
                leaves.add(i);
            }
        }
 
        for (int i = 0; i < n - 2; i++) {
            int leaf = leaves.first();
            leaves.remove(leaf);
            used[leaf] = true;
 
            int to = 0;
            for (int k = 0; k < g.get(leaf).size(); k++) {
                if (!used[g.get(leaf).get(k)]) {
                    to = g.get(leaf).get(k);
                }
            }
            ans[i] = to;
 
            if (pow[to] == 2) {
                leaves.add(to);
            }
            pow[to]--;
        }
        Arrays.stream(ans).forEach(i -> System.out.print((i + 1) + " "));
    }
}