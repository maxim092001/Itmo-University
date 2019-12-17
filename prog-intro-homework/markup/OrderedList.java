package markup;

import java.util.List;

public class OrderedList extends AbstractList {

    public OrderedList(List<ListItem> elements) {
        super(elements);
    }

    @Override
    public void toTex(StringBuilder stringBuilder) {
        super.toTex(stringBuilder, "\\begin{enumerate}", "\\end{enumerate}");
    }
}
