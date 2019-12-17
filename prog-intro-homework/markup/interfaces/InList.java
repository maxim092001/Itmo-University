package markup.interfaces;

public interface InList extends Texable {
    @Override
    void toTex(StringBuilder result);
}
