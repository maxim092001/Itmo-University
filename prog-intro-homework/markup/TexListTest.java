package markup;

import java.util.Map;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class TexListTest extends ListTest {
    private static final Map<String, String> TEX = Map.ofEntries(
            Map.entry("<em>", "\\emph{"),
            Map.entry("</em>", "}"),
            Map.entry("<strong>", "\\textbf{"),
            Map.entry("</strong>", "}"),
            Map.entry("<s>", "\\textst{"),
            Map.entry("</s>", "}"),
            Map.entry("<ul>", "\\begin{itemize}"),
            Map.entry("</ul>", "\\end{itemize}"),
            Map.entry("<ol>", "\\begin{enumerate}"),
            Map.entry("</ol>", "\\end{enumerate}"),
            Map.entry("<li>", "\\item "),
            Map.entry("</li>", "")
    );

    @Override
    protected void test(final Paragraph paragraph, final String expected) {
        test(paragraph::toTex, expected, TEX);
    }

    protected void test(UnorderedList list, final String expected) {
        test(list::toTex, expected, TEX);
    }

    protected void test(OrderedList list, final String expected) {
        test(list::toTex, expected, TEX);
    }

    public static void main(String[] args) {
        new TexListTest().run();
    }
}
