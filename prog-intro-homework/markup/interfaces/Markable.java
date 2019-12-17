package markup.interfaces;

public interface Markable extends Texable {
    void toMarkdown(StringBuilder stringBuilder);
}
