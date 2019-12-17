package markup;

import markup.interfaces.InParagraph;
import markup.interfaces.Markable;

import java.util.List;

public abstract class AbstractElement implements InParagraph {

    private List<InParagraph> markUpElements;

    protected AbstractElement(List<InParagraph> list) {
        this.markUpElements = list;
    }

    @Override
    public void toMarkdown(StringBuilder stringBuilder) {
        stringBuilder.append(getMarkdownBorder());
        for (InParagraph element : markUpElements) {
            element.toMarkdown(stringBuilder);
        }
        stringBuilder.append(getMarkdownBorder());
    }

    @Override
    public void toTex(StringBuilder stringBuilder) {
        stringBuilder.append(getTexPrefixBorder());
        for (InParagraph element : markUpElements) {
            element.toTex(stringBuilder);
        }
        stringBuilder.append(getTexPostfixBorder());
    }

    public abstract String getTexPrefixBorder();
    public abstract String getTexPostfixBorder();
    public abstract String getMarkdownBorder();
}
