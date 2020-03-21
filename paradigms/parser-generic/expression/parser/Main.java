package expression.parser;

import expression.exception.ParsingException;
import expression.operation.IntegerOperations;

public class Main {
    public static void main(String[] args) throws ParsingException {
        System.out.println(new ExpressionParser<>(new IntegerOperations(true)).parse("count -5").evaluate(0, 0, 0));
    }
}
