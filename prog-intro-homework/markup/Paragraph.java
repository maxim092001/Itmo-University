package markup;

import markup.interfaces.InList;
import markup.interfaces.InParagraph;
import markup.interfaces.Markable;
import markup.interfaces.Texable;

import java.util.List;

public class Paragraph implements Texable, Markable, InList {

    List<InParagraph> list;

    public Paragraph(List<InParagraph> list) {
        this.list = list;
    }

    @Override
    public void toMarkdown(StringBuilder stringBuilder) {
        for (InParagraph element : list) {
            element.toMarkdown(stringBuilder);
        }
    }

    @Override
    public void toTex(StringBuilder stringBuilder) {
        for (InParagraph element : list) {
            element.toTex(stringBuilder);
        }
    }
}
