package md2html;

import base.MainFilesChecker;
import base.Randomized;

import java.util.*;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class Md2HtmlTest {
    protected static Map<String, String> TAGS = new HashMap<>();
    static {
        TAGS.put("*", "em");
        TAGS.put("**", "strong");
        TAGS.put("_", "em");
        TAGS.put("__", "strong");
        TAGS.put("--", "s");
        TAGS.put("`", "code");
    }

    protected final Md2HtmlChecker checker = new Md2HtmlChecker("md2html.Md2Html");

    protected void test() {
        test(
                "# Заголовок первого уровня\n\n",
                "<h1>Заголовок первого уровня</h1>"
        );
        test(
                "## Второго\n\n",
                "<h2>Второго</h2>"
        );
        test(
                "### Третьего ## уровня\n\n",
                "<h3>Третьего ## уровня</h3>"
        );
        test(
                "#### Четвертого\n# Все еще четвертого\n\n",
                "<h4>Четвертого\n# Все еще четвертого</h4>"
        );
        test(
                "Этот абзац текста,\nсодержит две строки.",
                "<p>Этот абзац текста,\nсодержит две строки.</p>"
        );
        test(
                "    # Может показаться, что это заголовок.\nНо нет, это абзац начинающийся с `#`.\n\n",
                "<p>    # Может показаться, что это заголовок.\nНо нет, это абзац начинающийся с <code>#</code>.</p>"
        );
        test(
                "#И это не заголовок.\n\n",
                "<p>#И это не заголовок.</p>"
        );
        test(
                "###### Заголовки могут быть многострочными\n(и с пропуском заголовков предыдущих уровней)\n\n",
                "<h6>Заголовки могут быть многострочными\n(и с пропуском заголовков предыдущих уровней)</h6>"
        );
        test(
                "Мы все любим *выделять* текст _разными_ способами.\n**Сильное выделение**, используется гораздо реже,\nно __почему бы и нет__?\nНемного --зачеркивания-- еще ни кому не вредило.\nКод представляется элементом `code`.\n\n",
                "<p>Мы все любим <em>выделять</em> текст <em>разными</em> способами.\n<strong>Сильное выделение</strong>, используется гораздо реже,\nно <strong>почему бы и нет</strong>?\nНемного <s>зачеркивания</s> еще ни кому не вредило.\nКод представляется элементом <code>code</code>.</p>"
        );
        test(
                "Обратите внимание, как экранируются специальные\nHTML-символы, такие как `<`, `>` и `&`.\n\n",
                "<p>Обратите внимание, как экранируются специальные\nHTML-символы, такие как <code>&lt;</code>, <code>&gt;</code> и <code>&amp;</code>.</p>"
        );
        test(
                "Экранирование должно работать во всех местах: <>&.\n\n",
                "<p>Экранирование должно работать во всех местах: &lt;&gt;&amp;.</p>"
        );
        test(
                "Знаете ли вы, что в Markdown, одиночные * и _\nне означают выделение?\nОни так же могут быть заэкранированы\nпри помощи обратного слэша: \\*.",
                "<p>Знаете ли вы, что в Markdown, одиночные * и _\nне означают выделение?\nОни так же могут быть заэкранированы\nпри помощи обратного слэша: *.</p>"
        );
        test(
                "\n\n\nЛишние пустые строки должны игнорироваться.\n\n\n\n",
                "<p>Лишние пустые строки должны игнорироваться.</p>"
        );
        test(
                "Любите ли вы *вложеные __выделения__* так,\nкак __--люблю--__ их я?",
                "<p>Любите ли вы <em>вложеные <strong>выделения</strong></em> так,\nкак <strong><s>люблю</s></strong> их я?</p>"
        );

        test("# Заголовок первого уровня\n\n" +
                "## Второго\n\n" +
                "### Третьего ## уровня\n\n" +
                "#### Четвертого\n" +
                "# Все еще четвертого\n\n" +
                "Этот абзац текста,\n" +
                "содержит две строки.\n\n" +
                "    # Может показаться, что это заголовок.\n" +
                "Но нет, это абзац начинающийся с `#`.\n\n" +
                "#И это не заголовок.\n\n" +
                "###### Заголовки могут быть многострочными\n" +
                "(и с пропуском заголовков предыдущих уровней)\n\n" +
                "Мы все любим *выделять* текст _разными_ способами.\n" +
                "**Сильное выделение**, используется гораздо реже,\n" +
                "но __почему бы и нет__?\n" +
                "Немного --зачеркивания-- еще ни кому не вредило.\n" +
                "Код представляется элементом `code`.\n\n" +
                "Обратите внимание, как экранируются специальные\n" +
                "HTML-символы, такие как `<`, `>` и `&`.\n\n" +
                "Знаете ли вы, что в Markdown, одиночные * и _\n" +
                "не означают выделение?\n" +
                "Они так же могут быть заэкранированы\n" +
                "при помощи обратного слэша: \\*.\n\n\n\n" +
                "Лишние пустые строки должны игнорироваться.\n\n" +
                "Любите ли вы *вложеные __выделения__* так,\n" +
                "как __--люблю--__ их я?", "<h1>Заголовок первого уровня</h1>\n" +
                "<h2>Второго</h2>\n" +
                "<h3>Третьего ## уровня</h3>\n" +
                "<h4>Четвертого\n" +
                "# Все еще четвертого</h4>\n" +
                "<p>Этот абзац текста,\n" +
                "содержит две строки.</p>\n" +
                "<p>    # Может показаться, что это заголовок.\n" +
                "Но нет, это абзац начинающийся с <code>#</code>.</p>\n" +
                "<p>#И это не заголовок.</p>\n" +
                "<h6>Заголовки могут быть многострочными\n" +
                "(и с пропуском заголовков предыдущих уровней)</h6>\n" +
                "<p>Мы все любим <em>выделять</em> текст <em>разными</em> способами.\n" +
                "<strong>Сильное выделение</strong>, используется гораздо реже,\n" +
                "но <strong>почему бы и нет</strong>?\n" +
                "Немного <s>зачеркивания</s> еще ни кому не вредило.\n" +
                "Код представляется элементом <code>code</code>.</p>\n" +
                "<p>Обратите внимание, как экранируются специальные\n" +
                "HTML-символы, такие как <code>&lt;</code>, <code>&gt;</code> и <code>&amp;</code>.</p>\n" +
                "<p>Знаете ли вы, что в Markdown, одиночные * и _\n" +
                "не означают выделение?\n" +
                "Они так же могут быть заэкранированы\n" +
                "при помощи обратного слэша: *.</p>\n" +
                "<p>Лишние пустые строки должны игнорироваться.</p>\n" +
                "<p>Любите ли вы <em>вложеные <strong>выделения</strong></em> так,\n" +
                "как <strong><s>люблю</s></strong> их я?</p>\n");

        test("# Без перевода строки в конце", "<h1>Без перевода строки в конце</h1>");
        test("# Один перевод строки в конце\n", "<h1>Один перевод строки в конце</h1>");
        test("# Два перевода строки в конце\n\n", "<h1>Два перевода строки в конце</h1>");
        test(
                "Выделение может *начинаться на одной строке,\n а заканчиваться* на другой",
                "<p>Выделение может <em>начинаться на одной строке,\n а заканчиваться</em> на другой</p>"
        );
        test("# *Выделение* и `код` в заголовках", "<h1><em>Выделение</em> и <code>код</code> в заголовках</h1>");

        for (final String markup : TAGS.keySet()) {
            randomTest(3, 10, markup);
        }

        randomTest(100, 1000, "_", "**", "`", "--");
        randomTest(100, 1000, "*", "__", "`", "--");
    }

    protected void test(final String input, final String output) {
        checker.test(input, output);
    }

    protected void randomTest(final int paragraphs, final int length, final String... markup) {
        final StringBuilder input = new StringBuilder();
        final StringBuilder output = new StringBuilder();
        emptyLines(input);
        final List<String> markupList = new ArrayList<>(Arrays.asList(markup));
        for (int i = 0; i < paragraphs; i++) {
            final StringBuilder inputSB = new StringBuilder();
            paragraph(length, inputSB, output, markupList);
            input.append(inputSB);
            emptyLines(input);
        }
        test(input.toString(), output.toString());
    }

    private void paragraph(final int length, final StringBuilder input, final StringBuilder output, final List<String> markup) {
        final int h = checker.randomInt(0, 6);
        final String tag = h == 0 ? "p" : "h" + h;
        if (h > 0) {
            input.append(new String(new char[h]).replace('\0', '#')).append(" ");
        }

        open(output, tag);
        while (input.length() < length) {
            generate(markup, input, output);
            final String middle = checker.randomString(Randomized.ENGLISH);
            input.append(middle).append("\n");
            output.append(middle).append("\n");
        }
        output.setLength(output.length() - 1);
        close(output, tag);

        output.append("\n");
        input.append("\n");
    }

    private void randomSpace(final StringBuilder input, final StringBuilder output) {
        if (checker.random.nextBoolean()) {
            final String space = checker.random.nextBoolean() ? " " : "\n";
            input.append(space);
            output.append(space);
        }
    }

    protected void generate(final List<String> markup, final StringBuilder input, final StringBuilder output) {
        word(input, output);
        if (markup.isEmpty()) {
            return;
        }
        final String type = checker.randomItem(markup);

        markup.remove(type);
        if (TAGS.containsKey(type)) {
            ordinary(markup, input, output, type);
        } else {
            special(markup, input, output, type);
        }
        markup.add(type);
    }

    private void ordinary(final List<String> markup, final StringBuilder input, final StringBuilder output, final String type) {
        final String tag = TAGS.get(type);

        randomSpace(input, output);
        input.append(type);
        open(output, tag);

        word(input, output);
        generate(markup, input, output);
        word(input, output);

        input.append(type);
        close(output, tag);
        randomSpace(input, output);
    }

    protected void special(final List<String> markup, final StringBuilder input, final StringBuilder output, final String type) {
    }

    private void word(final StringBuilder input, final StringBuilder output) {
        final String word = checker.randomString(Randomized.ENGLISH);
        input.append(word);
        output.append(word);
    }

    private static void open(final StringBuilder output, final String tag) {
        output.append("<").append(tag).append(">");
    }

    private static void close(final StringBuilder output, final String tag) {
        output.append("</").append(tag).append(">");
    }

    private void emptyLines(final StringBuilder sb) {
        while (checker.random.nextBoolean()) {
            sb.append('\n');
        }
    }

    static class Md2HtmlChecker extends MainFilesChecker {
        public Md2HtmlChecker(final String className) {
            super(className);
        }

        private void test(final String input, final String output) {
            final List<String> run = runFiles(List.of(input.split("\n")));
            checkEquals(List.of(output.split("\n")), run);
        }
    }

    protected void run() {
        test();

        checker.printStatus(getClass());
    }

    public static void main(final String... args) {
        new Md2HtmlTest().run();
    }
}
