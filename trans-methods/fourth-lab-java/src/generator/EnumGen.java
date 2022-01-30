package generator;

import util.Block;
import util.Terminal;

import java.util.List;
import java.util.stream.Collectors;

public class EnumGen extends Generator<Terminal> {

    @Override
    public String generate(List<Terminal> blocks) {
        return String.format(
                """
                        public class GeneratedEnum {
                        public String text;
                        public EToken token;
                        public GeneratedEnum(EToken token, String text) {
                        this.text = text;
                        this.token = token;
                        }
                        enum EToken {%s, END, EPS;}
                        @Override
                        public String toString() {
                        return text;
                        }
                        }""",
                blocks.stream()
                        .filter(x -> !x.name.equals("SKIP") && !x.name.equals("EPS"))
                        .map(Terminal::nameToJava)
                        .collect(Collectors.joining(", ")));
    }
}
