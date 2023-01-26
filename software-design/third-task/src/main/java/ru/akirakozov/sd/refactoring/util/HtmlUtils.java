package ru.akirakozov.sd.refactoring.util;

public class HtmlUtils {
    public static String wrapHtml(final String s) {
        return String.format(
                "<html><body>%s</body></html>", s
        );
    }
}
