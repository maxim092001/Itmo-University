package expression;

public class Subtract extends AbstractExpression {

    public Subtract(PriorityExpression operand1, PriorityExpression operand2) {
        super(operand1, operand2,  "-");
    }

    @Override
    protected int operation(int a, int b) {
        return a - b;
    }

    @Override
    public int priority() {
        return 0;
    }

    @Override
    public boolean needBrackets() {
        return true;
    }
}
