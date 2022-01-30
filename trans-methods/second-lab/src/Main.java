import java.io.*;

public class Main {
    private static int index = 0;
    private static final String STRING_TO_PARSE = "(a or b) and c";
    private static final String FILE_NAME = "graph.txt";

    public static void main(String[] args) throws Exception {
        final Parser parser = new Parser();
        final Tree tree = parser.parse(STRING_TO_PARSE);

        try (final Writer writer = new FileWriter(FILE_NAME)) {
            writer.write(generateDot(tree));
        } catch (final IOException e) {
            System.err.printf("Error during I/O operation for dot generation: %s \n", e.getLocalizedMessage());
        }
    }

    private static String generateDot(final Tree t) {
        StringBuilder sb = new StringBuilder("digraph {");
        dfs(t, -1, 0, sb);
        sb.append("}");
        return sb.toString();
    }

    private static void dfs(final Tree t, final int previousId, final int currentId, final StringBuilder sb) {
        if (!t.getChildren().isEmpty()) {
            sb.append(String.format("\t%d [label = \"%s\"]\n", currentId, t.getVal()));
            if (previousId != -1) {
                sb.append(String.format("\t%d -> %d\n", previousId, currentId));
            }
            t.getChildren().forEach(c -> dfs(c, currentId, ++index, sb));
        } else {
            sb.append(String.format("\t%d [label = \"%s\"]\n", currentId, t.getVal()))
                    .append(String.format("\t%s -> %d\n", previousId, currentId));
            index++;
        }
    }
}