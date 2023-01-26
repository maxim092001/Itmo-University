import java.util.*;
import java.util.stream.Collectors;

public class Main {

  public static void main(String[] args) {
    final var sc = new Scanner(System.in);

    final var k = sc.nextInt();
    sc.nextLine();
    final var lambdas = Arrays.stream(sc.nextLine().split("\\s+"))
                            .map(Integer::parseInt)
                            .collect(Collectors.toList());
    final var alpha = sc.nextInt();
    final var n = sc.nextInt();

    final var cntMessages = new int[k];
    final var cntWords = new ArrayList<Map<String, Integer>>();
    final var allWords = new HashSet<String>();

    final var probs = new ArrayList<Map<String, Double>>();
    final var reversedProbs = new ArrayList<Map<String, Double>>();

    for (int i = 0; i < k; i++) {
      cntWords.add(new HashMap<>());
      probs.add(new HashMap<>());
      reversedProbs.add(new HashMap<>());
    }

    sc.nextLine();
    for (int i = 0; i < n; i++) {
      final var ln = sc.nextLine().split("\\s+");
      var c = Integer.parseInt(ln[0]) - 1;
      cntMessages[c] += 1;
      var set = new HashSet<>(Arrays.asList(ln).subList(2, ln.length));
      for (String word : set) {
        cntWords.get(c).put(word, cntWords.get(c).getOrDefault(word, 0) + 1);
        allWords.add(word);
      }
    }

    for (int c = 0; c < k; c++) {
      var dv = cntMessages[c] + 2 * alpha;
      var dvLog = Math.log(dv);
      for (String word : allWords) {
        probs.get(c).put(
            word,
            Math.log(cntWords.get(c).getOrDefault(word, 0) + alpha) - dvLog);
        reversedProbs.get(c).put(
            word, Math.log(dv - cntWords.get(c).getOrDefault(word, 0) - alpha) -
                      dvLog);
      }
    }

    var m = sc.nextInt();
    var logN = Math.log(n);

    sc.nextLine();
    for (int i = 0; i < m; i++) {
      final var ln = sc.nextLine().split("\\s+");
      var words = new HashSet<>(Arrays.asList(ln).subList(1, ln.length));
      var anss = new Double[k];
      for (int c = 0; c < k; c++) {
        if (cntMessages[c] == 0)
          continue;
        var p = Math.log(cntMessages[c]) - logN;
        anss[c] = Math.log(lambdas.get(c)) + p;
        for (String word : allWords) {
          if (words.contains(word)) {
            anss[c] += probs.get(c).getOrDefault(word, 0.0);
          } else {
            anss[c] += reversedProbs.get(c).getOrDefault(word, 0.0);
          }
        }
      }

      final Double mx = Arrays.stream(anss)
                            .filter(Objects::nonNull)
                            .max(Double::compareTo)
                            .get();

      var nAns = Arrays.stream(anss)
                     .map(x -> {
                       if (x != null) {
                         return Math.exp(x - mx);
                       } else {
                         return 0.0;
                       }
                     })
                     .collect(Collectors.toList());

      var sum = 0.0;
      for (Double d : nAns)
        sum += d;
      var newAns = new ArrayList<Double>();

      for (Double d : nAns) {
        newAns.add(d / sum);
      }

      newAns.forEach(x -> System.out.print(x + " "));
      System.out.println();
    }
  }
}
