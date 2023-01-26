import java.util.*;

public class Main {

  public static void main(String[] args) {
    final var sc = new Scanner(System.in);
    final var n = sc.nextInt();
    final var m = sc.nextInt();
    final var k = sc.nextInt();

    final List<List<Integer>> lst = new ArrayList<>();
    for (int i = 0; i < n; i++) {
      lst.add(new ArrayList<>());
    }
    for (int i = 0; i < n; i++) {
      final var t = sc.nextInt();
      lst.get(t - 1).add(i + 1);
    }
    int x = 0;

    final List<List<Integer>> ans = new ArrayList<>();
    for (int i = 0; i < n; i++) {
      ans.add(new ArrayList<>());
    }

    for (int i = 0; i < m; i++) {
      while (!lst.get(i).isEmpty()) {
        ans.get(x).add(lst.get(i).get(lst.get(i).size() - 1));
        lst.get(i).remove(lst.get(i).size() - 1);
        x = (x + 1) % k;
      }
    }

    for (int i = 0; i < k; i++) {
      System.out.print(ans.get(i).size() + " ");
      ans.get(i).forEach(z -> System.out.print(z + " "));
      System.out.println();
    }
  }
}
