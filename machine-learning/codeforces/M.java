import java.util.*;

public class Main {

  public static void main(String[] args) {
    final var sc = new Scanner(System.in);
    final var k = sc.nextInt();
    final var n = sc.nextInt();
    final Map<Integer, List<Integer>> ys = new HashMap<>();

    List<Pair> lst = new ArrayList<>();
    for (int i = 0; i < n; i++) {
      final var x = sc.nextInt();
      final var y = sc.nextInt();
      ys.put(y, new ArrayList<>());
      lst.add(new Pair(x, y));
    }

    for (final Pair p : lst) {
      ys.get(p.y).add(p.x);
    }

    long in = 0;

    for (final Integer key : ys.keySet()) {
      final var l = ys.get(key);
      l.sort(Integer::compareTo);
      long pref = 0;
      long suf = 0;
      for (final Integer el : l) {
        suf += el;
      }
      long pref_size = 0;
      for (final Integer el : l) {
        pref_size++;
        suf -= el;
        pref += el;
        in += (el * pref_size - pref) + (suf - (el * (l.size() - pref_size)));
      }
    }
    System.out.println(in);

    long pref_s = 0;
    long suf_s = 0;

    lst.sort((x, y) -> {
      if (x.x == y.x) {
        return Integer.compare(x.y, y.y);
      } else
        return Integer.compare(x.x, y.x);
    });

    final Map<Integer, Long> suf_size = new HashMap<>();
    final Map<Integer, Long> pref_size = new HashMap<>();
    final Map<Integer, Long> suf_el = new HashMap<>();
    final Map<Integer, Long> pref_el = new HashMap<>();

    for (final Pair p : lst) {
      var v = suf_size.getOrDefault(p.y, 0L);
      suf_size.put(p.y, v + 1);
      v = suf_el.getOrDefault(p.y, 0L);
      suf_el.put(p.y, p.x + v);
      suf_s += p.x;
    }

    long ex = 0;
    long idx = 0;
    for (final Pair p : lst) {
      idx++;
      var v = suf_size.getOrDefault(p.y, 0L);
      suf_size.put(p.y, v - 1);
      v = pref_size.getOrDefault(p.y, 0L);
      pref_size.put(p.y, v + 1);

      pref_s += p.x;
      suf_s -= p.x;

      v = suf_el.getOrDefault(p.y, 0L);
      suf_el.put(p.y, v - p.x);
      v = pref_el.getOrDefault(p.y, 0L);
      pref_el.put(p.y, v + p.x);

      ex += (idx - pref_size.get(p.y)) * p.x - (pref_s - pref_el.get(p.y)) +
            (suf_s - suf_el.get(p.y)) -
            ((lst.size() - idx) - suf_size.get(p.y)) * p.x;
    }
    System.out.println(ex);
  }

  private static class Pair {
    private final int x;
    private final int y;

    public Pair(int x, int y) {
      this.x = x;
      this.y = y;
    }
  }
}
