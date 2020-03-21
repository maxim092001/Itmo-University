package expression.parser;

public class Variable<T> implements CommonExpression<T> {

    private String variableName;

    public Variable(String variableName) {
        this.variableName = variableName;
    }


    @Override
    public T evaluate(T x) {
        return x;
    }

    @Override
    public T evaluate(T x, T y, T z) {
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
