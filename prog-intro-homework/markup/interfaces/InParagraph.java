package markup.interfaces;

public interface InParagraph extends Markable {
    @Override
    void toMarkdown(StringBuilder stringBuilder);
    @Override
    void toTex(StringBuilder result);
}
