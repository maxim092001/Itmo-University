package wordStat;

import base.Pair;
import base.Randomized;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class WordStatLastIndexTest {
    private final WordStatIndexChecker checker;

    public WordStatLastIndexTest(final String className) {
        checker = new WordStatIndexChecker(className);
    }

    public static void main(final String... args) {
        new WordStatLastIndexTest("WordStatLastIndex").run();
    }

    protected void run() {
        test();
        checker.printStatus();
    }

    private void test() {
        testPP(
                "To be, or not to be, that is the question:");
        testPP(
                "Monday's child is fair of face.",
                "Tuesday's child is full of grace.");
        testPP(
                "Шалтай-Болтай",
                "Сидел на стене.",
                "Шалтай-Болтай",
                "Свалился во сне."
        );

        randomTest(3, 10, 10, 3, Randomized.ENGLISH, WordStatChecker.SIMPLE_DELIMITERS);
        randomTest(10, 3, 5, 5, Randomized.RUSSIAN, WordStatChecker.SIMPLE_DELIMITERS);
        randomTest(3, 10, 10, 3, Randomized.GREEK, WordStatChecker.SIMPLE_DELIMITERS);
        randomTest(3, 10, 10, 3, WordStatChecker.DASH, WordStatChecker.SIMPLE_DELIMITERS);
        randomTest(3, 10, 10, 3, Randomized.ENGLISH, WordStatChecker.ADVANCED_DELIMITERS);
        randomTest(10, 3, 5, 5, Randomized.RUSSIAN, WordStatChecker.ADVANCED_DELIMITERS);
        randomTest(3, 10, 10, 3, Randomized.GREEK, WordStatChecker.ADVANCED_DELIMITERS);
        randomTest(3, 10, 10, 3, WordStatChecker.DASH, WordStatChecker.ADVANCED_DELIMITERS);
        randomTest(100, 1000, 1000, 1000, Randomized.RUSSIAN + Randomized.ENGLISH + Randomized.GREEK + WordStatChecker.DASH, WordStatChecker.ADVANCED_DELIMITERS);
    }

    private void randomTest(final int wordLength, final int totalWords, final int wordsPerLine, final int lines, final String chars, final String delimiters) {
        final String[] words = checker.generateWords(wordLength, totalWords, chars);
        final String[][] text = checker.generateTest(lines, words, wordsPerLine);
        checker.testPP(checker.input(text, delimiters), answer(text));
    }

    public void testPP(String... lines) {
        checker.testPP(lines, answer(Arrays.stream(lines).map(s -> s.split("[ ,.:]+")).toArray(String[][]::new)));
    }

    // Stream "magic" code. You do not expected to understand it
    protected List<Pair<String, String>> answer(final String[][] text) {
        final Map<String, Integer> totals = Arrays.stream(text)
                .flatMap(Arrays::stream)
                .map(String::toLowerCase)
                .collect(Collectors.toMap(Function.identity(), k -> 1, Integer::sum, LinkedHashMap::new));

        final Map<String, StringBuilder> firsts = totals.keySet().stream()
                .collect(Collectors.toMap(Function.identity(), k -> new StringBuilder()));
        Arrays.stream(text)
                .map(line -> IntStream.range(0, line.length).boxed()
                        .collect(Collectors.toMap(i -> line[i].toLowerCase(), i -> i + 1, Math::max, HashMap::new)))
                .forEach(line -> line.forEach((k, v) -> firsts.get(k).append(" ").append(v)));
        return totals.keySet().stream()
                .map(k -> Pair.of(k, totals.get(k) + "" + firsts.get(k)))
                .collect(Collectors.toList());
    }
}
