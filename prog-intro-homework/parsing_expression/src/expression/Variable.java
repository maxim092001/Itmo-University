package expression;


public class Variable implements CommonExpression {

    private String variableName;

    public Variable(String variableName) {
        this.variableName = variableName;
    }


    @Override
    public int evaluate(int x) {
        return x;
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
}
