package md2html;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class Md2HtmlParser {
    private MarkdownFileSource source;
    private int index = 0;
    private String curParagraph;
    final private Map<String, String> closeSequences = new HashMap<>();
    final private Map<Character, String> specialSymbols = new HashMap<>();
    final private Map<String, String> closeOpenTags = new HashMap<>();
    final private String[] markupSequences = {"**", "*", "__", "_", "--", "`", "++", "~"};

    public Md2HtmlParser(MarkdownFileSource source) {
        this.source = source;

        closeOpenTags.put("</strong>", "<strong>");
        closeOpenTags.put("</mark>", "<mark>");
        closeOpenTags.put("</code>", "<code>");
        closeOpenTags.put("</em>", "<em>");
        closeOpenTags.put("</u>", "<u>");
        closeOpenTags.put("</s>", "<s>");

        specialSymbols.put('&', "&amp;");
        specialSymbols.put('>', "&gt;");
        specialSymbols.put('<', "&lt;");

        closeSequences.put("**", "</strong>");
        closeSequences.put("__", "</strong>");
        closeSequences.put("*", "</em>");
        closeSequences.put("_", "</em>");
        closeSequences.put("--", "</s>");
        closeSequences.put("++", "</u>");
        closeSequences.put("`", "</code>");
        closeSequences.put("~", "</mark>");
        closeSequences.put(")", "");
        closeSequences.put("]", "");

    }

    private String convertCloseSequence(String sequence) {
        return closeSequences.get(sequence);
    }

    private String convertSpecialHtmlSymbols(char c) {
        return specialSymbols.containsKey(c) ? specialSymbols.get(c) : String.valueOf(c);
    }

    private String convertSpecialMarkdownSymbols() {
        StringBuilder convertedMarkdown = new StringBuilder();

        char currentChar = curParagraph.charAt(index);

        if (currentChar == '\\' || currentChar == '`' || currentChar == '_' || currentChar == '*') {
            convertedMarkdown.append(currentChar);
        } else {
            convertedMarkdown.append('\\').append(currentChar);
        }

        return convertedMarkdown.toString();
    }

    private String convertImage() {
        StringBuilder convertedImage = new StringBuilder();

        index += 2;
        String text = parseText("]", true);
        index++;
        String url = parseText(")", true);
        index--;
        convertedImage.append("<img alt='").append(text).append("' src='").append(url).append("'>");

        return convertedImage.toString();
    }

    private String convertLink() {
        StringBuilder convertedLink = new StringBuilder();

        index++;
        String link = parseText("]", false);
        index++;
        String url = parseText(")", true);
        convertedLink.append("<a href='").append(url).append("'>").append(link).append("</a>");
        index--;
        return convertedLink.toString();
    }


    private String convertMarkup() {
        StringBuilder convertedMarkup = new StringBuilder();

        String markupSequence = getMarkupSequence();
        index += markupSequence != null ? markupSequence.length() : 0;

        boolean toReturn = false;

        String parsedText = parseText(markupSequence, false);
        int closeSequencePosition = parsedText.length() - convertCloseSequence(markupSequence).length();

        if (closeSequencePosition > 0) {
            String closeSequence = parsedText.substring(closeSequencePosition);
            convertedMarkup.append(closeOpenTags.getOrDefault(closeSequence, markupSequence));
            index--;
        } else {
            convertedMarkup.append(markupSequence);
            toReturn = true;
        }

        if (toReturn) {
            index--;
        } else {
            convertedMarkup.append(parsedText);
        }
        return convertedMarkup.toString();
    }

    private String getMarkupSequence() {
        for (String sequence : markupSequences) {
            if (index + sequence.length() <= curParagraph.length() &&
                    curParagraph.substring(index, index + sequence.length()).equals(sequence)) {
                return sequence;
            }
        }
        return null;
    }

    private int parseHeadline() {

        while (index < curParagraph.length() && curParagraph.charAt(index) == '#') {
            index++;
        }

        if (Character.isWhitespace(curParagraph.charAt(index))) {
            return index++;
        }

        return 0;
    }

    private boolean checkForStopSequence(String stopSequence) {
        if (stopSequence == null || index + stopSequence.length() > curParagraph.length()) {
            return false;
        }
        return curParagraph.substring(index, index + stopSequence.length()).equals(stopSequence);
    }

    private String parseText(String closeSequence, boolean inImageOrInLink) {
        StringBuilder parsedText = new StringBuilder();
        while (index < curParagraph.length()) {
            if (index > 0 && curParagraph.charAt(index - 1) == '\\') {
                parsedText.append(convertSpecialMarkdownSymbols());
            } else if (checkForStopSequence(closeSequence)) {
                parsedText.append(convertCloseSequence(closeSequence));
                index += closeSequence.length();
                return parsedText.toString();
            } else if (curParagraph.charAt(index) == '!') {
                parsedText.append(convertImage());
            } else if (!inImageOrInLink && curParagraph.charAt(index) == '[') {
                parsedText.append(convertLink());
            } else if (!inImageOrInLink && getMarkupSequence() != null) {
                parsedText.append(convertMarkup());
            } else if (curParagraph.charAt(index) == '\\') {
                if (index + 1 >= curParagraph.length()) {
                    parsedText.append('\\');
                }
            } else {
                parsedText.append(convertSpecialHtmlSymbols(curParagraph.charAt(index)));
            }
            index++;
        }

        return parsedText.toString();
    }

    private String parseParagraph() {
        index = 0;
        StringBuilder parsedParagraph = new StringBuilder();

        int countOfHashes = parseHeadline();

        if (countOfHashes == 0) {
            parsedParagraph.append("<p>");
            parsedParagraph.append(curParagraph, 0, index);
            parsedParagraph.append(parseText(null, false));
            parsedParagraph.append("</p>");
        } else {
            parsedParagraph.append("<h").append(countOfHashes).append(">");
            parsedParagraph.append(parseText(null, false));
            parsedParagraph.append("</h").append(countOfHashes).append(">");
        }
        return parsedParagraph.toString();
    }

    public String parse() throws IOException {
        StringBuilder parseMarkdown = new StringBuilder();

        while (!source.nextParagraph().isEmpty()) {
            curParagraph = source.getParagraph();
            parseMarkdown.append(parseParagraph()).append('\n');
        }
        return parseMarkdown.toString();
    }
}
