package expression.parser;

public class Const<T> implements CommonExpression<T> {


    private T constValue;

    public Const(T constValue) {
        this.constValue = constValue;
    }

    @Override
    public T evaluate(T x) {
        return this.constValue;
    }

    @Override
    public T evaluate(T x, T y, T z) {
        return this.constValue;
    }
}
