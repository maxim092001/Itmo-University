package generator;

import util.Terminal;

import java.util.List;
import java.util.stream.Collectors;

public class LexerGen extends Generator<Terminal> {

    private static final String SKIP = "SKIP";

    @Override
    public String generate(List<Terminal> blocks) {
        return String.format("""
                        %s
                                
                        public class GeneratedLexer implements Iterator<GeneratedEnum>, Iterable<GeneratedEnum> {
                        %s

                        %s
                        private int curStart, curEnd;
                        private String text;
                        private boolean hasNext;

                        public int position() { return curEnd; }
                                
                        @Override
                        public Iterator<GeneratedEnum> iterator() {
                        return this;
                        }

                        public GeneratedLexer(String text) {
                        this.text = text;
                        curStart = 0;
                        curEnd = 0;
                        hasNext = true;
                        }
                                
                        @Override
                        public boolean hasNext() {
                        return hasNext;
                        }
                                
                        private boolean matchLookingAt() {
                        if (matcher.lookingAt()) {
                        curStart = curEnd;
                        curEnd = curStart + matcher.end();
                        matcher.reset(text.substring(curEnd));
                        return true;
                        }
                        return false;
                        }
                                
                        @Override
                        public GeneratedEnum next() {
                        curStart = curEnd;
                        matcher.usePattern(skip);
                        matcher.reset(text.substring(curStart));
                        matchLookingAt();
                        for (var t : GeneratedEnum.EToken.values()) {
                        if (t == GeneratedEnum.EToken.END || t == GeneratedEnum.EToken.EPS) {
                        continue;
                        }
                        matcher.usePattern(patterns.get(t));
                        if (matchLookingAt()) {
                        return new GeneratedEnum(t, text.substring(curStart, curEnd));
                        }
                        }
                        if (curEnd != text.length()) {
                        throw new Error();
                        }
                        hasNext = false;
                        return new GeneratedEnum(GeneratedEnum.EToken.END, null);
                        }
                        }""",
                generateImports(),
                generateTokenMap(blocks),
                generateSkip(blocks));
    }

    private String generateImports() {
        return generateImports(List.of("java.util.Map", "java.util.HashMap", "java.util.regex.*", "java.util.Iterator"));
    }

    private String generateTokenMap(final List<Terminal> blocks) {
        return String.format("private final Map<GeneratedEnum.EToken, Pattern> patterns = Map.ofEntries(\n%s\n);",
                blocks.stream()
                        .filter(x -> !x.name.equals(SKIP))
                        .map(x ->
                                String.format("Map.entry(GeneratedEnum.EToken.%s, %s)",
                                        x.nameToJava(), x.patternToJava())).collect(Collectors.joining(",\n")));
    }

    private String generateSkip(final List<Terminal> blocks) {
        return String.format("private final Pattern skip = Pattern.compile(\"[%s]+\");" +
                        "\nprivate final Matcher matcher = skip.matcher(\"\");",
                blocks.stream().filter(x -> x.name.equals(SKIP)).findAny().orElseThrow().pattern);
    }

}