import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;
import java.util.TreeSet;
 
/**
 * @author Grankin Maxim (maximgran@gmail.com) at 21:58 05.12.2020
 */
public class F {
    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        int n = in.nextInt();
        int[] prufer = new int[n - 2];
        int[] pow = new int[n];
        for (int i = 0; i < n; i++) {
            pow[i] = 1;
        }
        for (int i = 0; i < n - 2; i++) {
            prufer[i] = in.nextInt();
            prufer[i]--;
            pow[prufer[i]]++;
        }
 
        TreeSet<Integer> leaves = new TreeSet<>();
        for (int i = 0; i < n; i++) {
            if (pow[i] == 1) {
                leaves.add(i);
            }
        }
 
        List<Edge> lst = new ArrayList<>();
        for (int i = 0; i < n - 2; i++) {
            int leaf = leaves.first();
            leaves.remove(leaf);
            int to = prufer[i];
 
            lst.add(new Edge(leaf, to));
            if (pow[to] == 2) {
                leaves.add(to);
            }
            pow[to]--;
        }
 
        lst.add(new Edge(leaves.first(), leaves.last()));
 
        lst.forEach(System.out::println);
    }
 
    private static class Edge {
        int x;
        int y;
 
        public Edge(final int x, final int y) {
            this.x = x;
            this.y = y;
        }
 
        @Override
        public String toString() {
            return (x + 1) + " " + (y + 1);
        }
    }
}