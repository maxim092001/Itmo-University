package markup;

import markup.interfaces.InList;

import java.util.List;

public abstract class AbstractList implements InList {

    private List<ListItem> elements;

    public AbstractList(List<ListItem> elements) {
        this.elements = elements;
    }

    public void toTex(StringBuilder stringBuilder, String leftBorder, String rightBorder) {
        stringBuilder.append(leftBorder);
        for (ListItem element : elements) {
            element.toTex(stringBuilder);
        }
        stringBuilder.append(rightBorder);
    }
}
