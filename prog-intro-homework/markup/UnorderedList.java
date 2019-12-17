package markup;

import java.util.List;

public class UnorderedList extends AbstractList {
    public UnorderedList(List<ListItem> elements) {
        super(elements);
    }

    @Override
    public void toTex(StringBuilder stringBuilder) {
        super.toTex(stringBuilder, "\\begin{itemize}", "\\end{itemize}");
    }
}
