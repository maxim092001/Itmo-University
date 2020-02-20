package expression.parser;

import exception.ParsingException;
import expression.TripleExpression;

public interface Parser {
    TripleExpression parse(String expression) throws ParsingException;
}
