package markup;

import markup.interfaces.InList;
import markup.interfaces.Texable;

import java.util.List;

public class ListItem implements Texable {

    private List<InList> elements;

    public ListItem(List<InList> elements) {
        this.elements = elements;
    }

    @Override
    public void toTex(StringBuilder stringBuilder) {
        stringBuilder.append("\\item ");
        for (InList element : elements) {
            element.toTex(stringBuilder);
        }
    }

}
