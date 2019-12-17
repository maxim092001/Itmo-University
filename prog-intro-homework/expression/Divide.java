package expression;

public class Divide extends AbstractExpression {

    public Divide(PriorityExpression firstOperand, PriorityExpression secondOperand) {
        super(firstOperand, secondOperand, "/");
    }

    @Override
    protected int operation(int a, int b) {
        return a / b;
    }

    @Override
    public int priority() {
        return 1;
    }

    @Override
    public boolean needBrackets() {
        return true;
    }
}
