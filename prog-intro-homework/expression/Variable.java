package expression;

public class Variable implements PriorityExpression {

    private String variableName;

    public Variable(String variableName) {
        this.variableName = variableName;
    }


    @Override
    public int evaluate(int x) {
        return x;
    }


    public String toString() {
        return variableName;
    }

    @Override
    public boolean equals(Object checkOperand) {
        if (checkOperand == null || checkOperand.getClass() != Variable.class) {
            return false;
        }
        return variableName.equals(checkOperand.toString());
    }

    public int hashCode() {
        return variableName.hashCode();
    }

    @Override
    public int evaluate(int x, int y, int z) {
        switch (variableName) {
            case "x":
                return x;
            case "y":
                return y;
            default:
                return z;
        }
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
