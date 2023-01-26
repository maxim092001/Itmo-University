import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Scanner;

public class Main {

  private static final List<List<Integer>> res = new ArrayList<>();

  public static void main(String[] args) {
    final var sc = new Scanner(System.in);
    final var m = sc.nextInt();

    gen(m, 0, new ArrayList<>());

    final var zeros = new ArrayList<Integer>();
    final var ones = new ArrayList<Integer>();

    for (int i = 0; i < res.size(); i++) {
      var temp = sc.nextInt();
      if (temp == 0) {
        zeros.add(i);
      } else {
        ones.add(i);
      }
    }

    if (ones.size() > 512) {
      System.out.println("2");
      System.out.println(zeros.size() + " "
                         + "1");
      printRes(zeros);
      for (final Integer i : zeros) {
        System.out.print("-1 ");
      }
      System.out.print("0.5");
    } else {
      if (ones.isEmpty()) {
        System.out.println("1");
        System.out.println("1");

        for (int i = 0; i < m; i++) {
          System.out.print("0 ");
        }

        System.out.print("-0.5");
      } else {
        System.out.println("2");
        System.out.println(ones.size() + " "
                           + "1");

        printRes(ones);
        for (final Integer i : ones) {
          System.out.print("1 ");
        }
        System.out.print("-0.5");
      }
    }
  }

  private static void printRes(final List<Integer> idx) {
    for (final Integer i : idx) {
      var counter = 0;
      for (final Integer x : res.get(i)) {
        if (x == 1) {

          System.out.print("1 ");
          counter++;
        } else {
          System.out.print("-1000000000 ");
        }
      }
      System.out.println(0.5 - counter);
    }
  }

  private static void gen(final Integer m, final Integer cur,
                          final List<Integer> curRes) {
    if (Objects.equals(m, cur)) {
      res.add(curRes);
    } else {
      var zeroCurRes = new ArrayList<>(curRes);
      zeroCurRes.add(0);
      gen(m, cur + 1, zeroCurRes);
      var oneCurRes = new ArrayList<>(curRes);
      oneCurRes.add(1);
      gen(m, cur + 1, oneCurRes);
    }
  }
}
