package md2html;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class Md2HtmlUnderlineTest extends Md2HtmlTest {
    static {
        TAGS.put("++", "u");
    }

    protected void test() {
        test("++подчеркивание++", "<p><u>подчеркивание</u></p>");
        super.test();

        randomTest(100, 1000, "_", "**", "`", "--", "++");
        randomTest(100, 1000, "*", "__", "`", "--", "++");
        randomTest(100, 200000, "*", "__", "`", "--", "++");
    }

    public static void main(final String... args) {
        new Md2HtmlUnderlineTest().run();
    }
}
