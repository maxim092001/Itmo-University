package util;

public class Import implements Block {
    public String text;

    public Import(String text) {
        this.text = text;
    }

    @Override
    public String toString() {
        return text;
    }

    @Override
    public String toJava() {
        return toString();
    }
}
