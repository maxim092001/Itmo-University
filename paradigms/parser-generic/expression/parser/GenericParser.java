package expression.parser;

import expression.exception.ParsingException;

public interface GenericParser<T> {
    GenericTripleExpression<T> parse(String expression) throws ParsingException;
}
