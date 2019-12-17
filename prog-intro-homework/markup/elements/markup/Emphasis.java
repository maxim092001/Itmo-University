package markup.elements.markup;

import markup.AbstractElement;
import markup.interfaces.InParagraph;

import java.util.List;

public class Emphasis extends AbstractElement {

    public Emphasis(List<InParagraph> list) {
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
        return "\\emph{";
    }

    @Override
    public String getTexPostfixBorder() {
        return "}";
    }

    @Override
    public String getMarkdownBorder() {
        return "*";
    }
}
