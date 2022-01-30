package util;

import java.util.Set;

public class Terminal implements Block {
    public String name;
    public String pattern;

    @Override
    public String toString() {
        return String.format("%s = %s", name, pattern);
    }

    @Override
    public String toJava() {
        return this.toString();
    }

    public String patternToJava() {
        return String.format("Pattern.compile(\"%s\")", pattern);
    }

    public String nameToJava() {
        return name;
    }
}
