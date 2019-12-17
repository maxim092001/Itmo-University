package md2html;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class Md2HtmlMarkTest extends Md2HtmlTest {
    static {
        TAGS.put("~", "mark");
    }

    protected void test() {
        test("~выделение~", "<p><mark>выделение</mark></p>");
        super.test();

        randomTest(100, 1000, "_", "**", "`", "--", "~");
        randomTest(100, 1000, "*", "__", "`", "--", "~");
        randomTest(100, 200000, "*", "__", "`", "--", "~");
    }

    public static void main(final String... args) {
        new Md2HtmlMarkTest().run();
    }
}
