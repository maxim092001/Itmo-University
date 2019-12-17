package md2html;

import java.util.List;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class Md2HtmlLinkTest extends Md2HtmlTest {
    protected void test() {
        test("[ссылок с _выделением_](https://kgeorgiy.info)", "<p><a href='https://kgeorgiy.info'>ссылок с <em>выделением</em></a></p>");
        test("[ссылка с __выделением__](https://kgeorgiy.info)", "<p><a href='https://kgeorgiy.info'>ссылка с <strong>выделением</strong></a></p>");
        test("[ссылка без выделения](https://kgeorgiy.info)", "<p><a href='https://kgeorgiy.info'>ссылка без выделения</a></p>");
        test("[ссылка без выделения](https://hello__kgeorgiy.info)", "<p><a href='https://hello__kgeorgiy.info'>ссылка без выделения</a></p>");
        test("_выделение [ссылка с __выделением__](https://kgeorgiy.info)_", "<p><em>выделение <a href='https://kgeorgiy.info'>ссылка с <strong>выделением</strong></a></em></p>");
        super.test();

        randomTest(100, 1000, "_", "**", "`", "--", "[");
        randomTest(100, 1000, "*", "__", "`", "--", "[");
        randomTest(100, 200000, "*", "__", "`", "--", "[");
    }

    @Override
    protected void special(final List<String> markup, final StringBuilder input, final StringBuilder output, final String type) {
        final StringBuilder in = new StringBuilder();
        final StringBuilder out = new StringBuilder();
        generate(markup, in, out);

        final StringBuilder href = new StringBuilder();
        generate(markup, href, new StringBuilder());

        input.append("[").append(in).append("](").append(href).append(')');
        output.append("<a href='").append(href).append("'>").append(out).append("</a>");
    }

    public static void main(final String... args) {
        new Md2HtmlLinkTest().run();
    }
}
