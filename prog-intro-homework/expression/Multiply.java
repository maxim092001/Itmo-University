package expression;

public class Multiply extends AbstractExpression {

    public Multiply(PriorityExpression operand1, PriorityExpression operand2) {
        super(operand1, operand2, "*");
    }

    @Override
    protected int operation(int a, int b) {
        return a * b;
    }

    @Override
    public int priority() {
        return 1;
    }

    @Override
    public boolean needBrackets() {
        return false;
    }
}
