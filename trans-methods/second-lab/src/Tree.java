import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Tree {

    private final String val;
    private List<Tree> children;

    private Tree(final String val, final List<Tree> children) {
        this.val = val;
        this.children = children;
    }

    public static Tree of(final String val) {
        return new Tree(val, List.of());
    }

    public static Tree of(final String val, final Tree ...children) {
        return new Tree(val, Arrays.asList(children));
    }

    public String getVal() {
        return val;
    }

    public List<Tree> getChildren() {
        return children;
    }
}
