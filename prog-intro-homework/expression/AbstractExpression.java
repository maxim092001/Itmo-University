package expression;

public abstract class AbstractExpression implements PriorityExpression {

    private String tag;
    private final static int PRIME = 7727;
    private PriorityExpression operand1, operand2;

    public AbstractExpression(PriorityExpression operand1, PriorityExpression operand2, String tag) {
        this.operand1 = operand1;
        this.operand2 = operand2;
        this.tag = tag;
    }

    abstract protected int operation(int a, int b);

    @Override
    public int evaluate(int x, int y, int z) {
        return operation(operand1.evaluate(x, y, z), operand2.evaluate(x, y, z));
    }

    @Override
    public int evaluate(int x) {
        return operation(operand1.evaluate(x), operand2.evaluate(x));
    }

    public boolean equals(Object checkOperand) {
        if (checkOperand == null || checkOperand.getClass() != this.getClass()) {
            return false;
        }
        AbstractExpression abstractExpression = (AbstractExpression) checkOperand;
        return this.operand1.equals(abstractExpression.operand1)
                && this.operand2.equals(abstractExpression.operand2);
    }

    @Override
    public String toString() {
        return "(" + operand1 + " " + tag + " " + operand2 + ")";
    }

    private String takeBrackets(final boolean brackets, final PriorityExpression nowOperand) {
        if (brackets) {
            return '(' + nowOperand.toMiniString() + ')';
        } else {
            return nowOperand.toMiniString();
        }
    }

    @Override
    public String toMiniString() {
        return takeBrackets(operand1.priority() < priority(), operand1)
                + ' ' + tag + ' ' + takeBrackets(operand2.priority() < priority()
                || priority() == operand2.priority()
                && (this.needBrackets() || operand2.needBrackets()), operand2);
    }

    @Override
    public int hashCode() {
        int result = 1;
        result = PRIME * result + operand1.hashCode();
        result = PRIME * result + operand2.hashCode();
        result = PRIME * result + getClass().hashCode();
        return result;
    }
}
