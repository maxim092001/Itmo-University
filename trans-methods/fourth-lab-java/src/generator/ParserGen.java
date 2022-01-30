package generator;

import util.Block;
import util.NonTerminal;

import java.util.List;
import java.util.stream.Collectors;

public class ParserGen extends Generator<NonTerminal> {

    @Override
    public String generate(List<NonTerminal> blocks) {
        return String.format("""
                        %s
                        public class GeneratedParser {
                        public GeneratedEnum curToken;
                        public final GeneratedLexer lexer;
                        %s
                        %s
                        
                        
                        %s
                        }""",
                generateImports(List.of("java.text.ParseException", "util.base.*")),
                generateConstructor(),
                blocks.stream()
                        .map(Block::toJava)
                        .collect(Collectors.joining("\n")),
                generateTreeNodeClass());
    }

    private String generateConstructor() {
        return "public GeneratedParser(String text) {\nlexer = new GeneratedLexer(text);\ncurToken = lexer.next();\n}";
    }

    private String generateTreeNodeClass() {
        return """
                public static class Node extends BaseNode {
                public Node(String name) {
                super(name);
                }
                }
                """;
    }
}
