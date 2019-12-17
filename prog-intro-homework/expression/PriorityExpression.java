package expression;

public interface PriorityExpression extends Expression, TripleExpression {
    int priority();
    boolean needBrackets();
}
