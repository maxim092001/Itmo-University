package markup.elements.text;

import markup.interfaces.InParagraph;

public class Text implements InParagraph {

    private String text;

    public Text(String text) {
        this.text = text;
    }

    @Override
    public void toMarkdown(StringBuilder stringBuilder) {
        stringBuilder.append(text);
    }

    @Override
    public void toTex(StringBuilder result) {
        result.append(text);
    }
}
