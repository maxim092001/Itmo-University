import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.nio.charset.StandardCharsets;
import java.util.LinkedHashMap;
import java.util.Map;

class WordStatIndexChecker implements Checker {
    @Override
    public boolean isWordCharacter(char c) {
        return Character.getType(c) == Character.DASH_PUNCTUATION
                || c == '\''
                || Character.isLetter(c);
    }
}

public class WordStatLastIndex {

    private static Checker wordStatIndexChecker = new WordStatIndexChecker();

    private static int wordIndex = 0;

    private static void wordsCnt(String text, Map<String, Words> mapCnt) {
        text = text.toLowerCase();
        wordIndex++;
        mapCnt.putIfAbsent(text, new Words());
        mapCnt.get(text).wordsInEachLine.add(wordIndex);
        mapCnt.get(text).wordsSize++;
    }

    private static class Words {
        IntList wordsInEachLine;
        int wordsSize;

        Words() {
            wordsInEachLine = new IntList();
            wordsSize = 0;
        }
    }

    public static void main(String[] args) {
        Map<String, Words> mapCnt = new LinkedHashMap<>();
        Map<String, Words> ans = new LinkedHashMap<>();
        try {
            Scanner in = new Scanner(new BufferedReader(new FileReader(new File(args[0]))), wordStatIndexChecker);
            while (in.hasNext()) {
                wordsCnt(in.nextWord(), mapCnt);
                if (in.isEndOfLine()) {
                    wordIndex = 0;
                    for (Map.Entry<String, Words> el : mapCnt.entrySet()) {
                        ans.putIfAbsent(el.getKey(), new Words());
                        ans.get(el.getKey()).wordsInEachLine.add(el.getValue().wordsInEachLine.back());
                        ans.get(el.getKey()).wordsSize += el.getValue().wordsSize;
                    }
                    mapCnt = new LinkedHashMap<>();
                }
            }
        } catch (FileNotFoundException e) {
            System.out.println("Input file not found: " + e.getMessage());
            return;
        } catch (IOException e) {
            System.out.println("Input error: " + e.getMessage());
            return;
        }

        try {
            try (PrintWriter out = new PrintWriter(
                    new BufferedWriter(
                            new OutputStreamWriter(
                                    new FileOutputStream(new File(args[1])),
                                    StandardCharsets.UTF_8))
            )) {

                for (Map.Entry<String, Words> el : ans.entrySet()) {
                    out.print(el.getKey() + " " + el.getValue().wordsSize);
                    for (Integer i : el.getValue().wordsInEachLine) {
                        out.print(" " + i);
                    }
                    out.println();
                }
            }
        } catch (FileNotFoundException e) {
            System.err.println("File not found: " + e.getMessage());
        }
    }

}

