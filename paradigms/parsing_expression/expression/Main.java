package expression;

import exception.ParsingException;
import expression.parser.ExpressionParser;

public class Main {
    public static void main(String[] args) throws ParsingException {
        System.out.println(new ExpressionParser().parse("(1**1000000000**1000000000**1000000000**1000000000**1000000000").evaluate(0, 0,0));
    }
}
