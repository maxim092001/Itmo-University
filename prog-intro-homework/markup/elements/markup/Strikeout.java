package markup.elements.markup;

import markup.AbstractElement;
import markup.interfaces.InParagraph;

import java.util.List;

public class Strikeout extends AbstractElement {

    public Strikeout(List<InParagraph> list) {
        super(list);
    }

    @Override
    public void toMarkdown(StringBuilder stringBuilder) {
        super.toMarkdown(stringBuilder);
    }

    @Override
    public void toTex(StringBuilder stringBuilder) {
        super.toTex(stringBuilder);
    }

    @Override
    public String getTexPrefixBorder() {
        return "\\textst{";
    }

    @Override
    public String getTexPostfixBorder() {
        return "}";
    }

    @Override
    public String getMarkdownBorder() {
        return "~";
    }
}
