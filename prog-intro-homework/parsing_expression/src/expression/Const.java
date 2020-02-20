package expression;

public class Const implements CommonExpression {


    private int constValue;

    public Const(int constValue) {
        this.constValue = constValue;
    }

    @Override
    public int evaluate(int x) {
        return this.constValue;
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return this.constValue;
    }
}
