package util.base;

import java.util.ArrayList;
import java.util.List;

public abstract class BaseNode {
    public List<BaseNode> children;
    public String name;

    public BaseNode(String name) {
        this.name = name;
        children = new ArrayList<>();
    }
}
