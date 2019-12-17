package expression;

public class Const implements PriorityExpression {

    private int x;

    public Const(int x) {
        this.x = x;
    }

    @Override
    public int evaluate(int x) {
        return this.x;
    }

    @Override
    public String toString() {
        return Integer.toString(this.x);
    }

    @Override
    public boolean equals(Object checkOperand) {

        if (checkOperand == null || checkOperand.getClass() != Const.class) {
            return false;
        }

        return this.toString().equals(checkOperand.toString());
    }

    @Override
    public int hashCode() {
        return this.x;
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return this.x;
    }

    @Override
    public int priority() {
        return 2;
    }

    @Override
    public boolean needBrackets() {
        return false;
    }
}
