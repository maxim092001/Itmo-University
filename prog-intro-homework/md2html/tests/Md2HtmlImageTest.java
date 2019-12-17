package md2html;

import java.util.List;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class Md2HtmlImageTest extends Md2HtmlTest {
    protected void test() {
        test("![картинок](http://www.ifmo.ru/images/menu/small/p10.jpg)", "<p><img alt='картинок' src='http://www.ifmo.ru/images/menu/small/p10.jpg'></p>");
        test("![картинка](https://kgeorgiy.info)", "<p><img alt='картинка' src='https://kgeorgiy.info'></p>");
        test("![картинка с __псевдо-выделением__](https://kgeorgiy.info)", "<p><img alt='картинка с __псевдо-выделением__' src='https://kgeorgiy.info'></p>");
        super.test();

        randomTest(100, 1000, "_", "**", "`", "--", "![");
        randomTest(100, 1000, "*", "__", "`", "--", "![");
        randomTest(100, 200000, "*", "__", "`", "--", "![");
    }

    @Override
    protected void special(final List<String> markup, final StringBuilder input, final StringBuilder output, final String type) {
        final StringBuilder alt = new StringBuilder();
        final StringBuilder src = new StringBuilder();

        generate(markup, alt, new StringBuilder());
        generate(markup, src, new StringBuilder());

        input.append("![").append(alt).append("](").append(src).append(')');
        output.append("<img alt='").append(alt).append("' src='").append(src).append("'>");
    }

    public static void main(final String... args) {
        new Md2HtmlImageTest().run();
    }
}
