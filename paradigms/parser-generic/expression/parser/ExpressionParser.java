package expression.parser;

import expression.exception.ArgumentException;
import expression.exception.ConstFormatException;
import expression.exception.IllegalOperation;
import expression.exception.ParenthesisException;
import expression.exception.ParsingException;
import expression.exception.VariableNameException;
import expression.operation.Operations;

import java.util.Set;

public class ExpressionParser<T> implements GenericParser<T> {

    private ExpressionSource source;
    private char currentChar;
    private final Set<Character> allowedChars = Set.of(
            '+',
            '-',
            '*',
            '/',
            ')',
            '(',
            '\0'
    );

    private final Set<String> allowVariables = Set.of(
            "x",
            "y",
            "z"
    );

    private Operations<T> operations;

    public ExpressionParser(Operations<T> operations) {
        this.operations = operations;
    }

    private void nextChar() {
        if (source.hasNext()) {
            currentChar = source.next();
        } else {
            currentChar = '\0';
        }
    }

    private boolean isEquals(char x) {
        skipWhiteSpace();
        if (currentChar == x) {
            nextChar();
            return true;
        }
        return false;
    }

    private boolean isLegalVariableName(String s) {
        return allowVariables.contains(s);
    }

    private void skipWhiteSpace() {
        while (Character.isWhitespace(currentChar)) {
            nextChar();
        }
    }

    private Const<T> parseConst(String sourcePrefix) throws ParsingException {
        StringBuilder number = new StringBuilder(sourcePrefix);
        while (Character.isDigit(currentChar)) {
            number.append(currentChar);
            nextChar();
        }
        skipWhiteSpace();
        try {
            return new Const<>(operations.parseValue(number.toString()));
        } catch (NumberFormatException e) {
            throw new ConstFormatException("Illegal const format", source.getCurrentPosition());
        }
    }

    private String parseStringVal() {
        skipWhiteSpace();
        StringBuilder str = new StringBuilder();
        while (Character.isLetter(currentChar)) {
            str.append(currentChar);
            nextChar();
        }
        return str.toString();
    }

    @Override
    public GenericTripleExpression<T> parse(String expression) throws ParsingException {
        source = new StringSource(expression);
        nextChar();
        skipWhiteSpace();
        CommonExpression<T> result = minMax();
        skipWhiteSpace();
        if (currentChar == ')') {
            throw new ParenthesisException("Parenthesis error", source.getCurrentPosition());
        } else if (currentChar != '\0') {
            throw new IllegalOperation("Illegal expression.operation error", source.getCurrentPosition());
        }
        return result;
    }


    private CommonExpression<T> addSub() throws ParsingException {
        CommonExpression<T> result = mulDiv();
        skipWhiteSpace();
        while (true) {
            if (isEquals('+')) {
                result = new Add<>(result, mulDiv(), operations);
            } else if (isEquals('-')) {
                result = new Subtract<>(result, mulDiv(), operations);
            } else {
                return result;
            }
        }
    }


    private CommonExpression<T> mulDiv() throws ParsingException {
        CommonExpression<T> result = number();
        skipWhiteSpace();
        while (true) {
            if (isEquals('*')) {
                result = new Multiply<>(result, number(), operations);
            } else if (isEquals('/')) {
                result = new Divide<>(result, number(), operations);
            } else {
                return result;
            }
        }
    }


    private CommonExpression<T> minMax() throws ParsingException {
        CommonExpression<T> result = addSub();
        skipWhiteSpace();
        while (true) {
            String s = parseStringVal();
            if (s.equals("min")) {
                result = new Min<>(result, addSub(), operations);
            } else if (s.equals("max")) {
                result = new Max<>(result, addSub(), operations);
            } else {
                return result;
            }
        }
    }

    private CommonExpression<T> number() throws ParsingException {
        CommonExpression<T> result;
        skipWhiteSpace();
        if (isEquals('(')) {
            result = addSub();
            if (!isEquals(')')) {
                throw new IllegalOperation("Illegal Operation", source.getCurrentPosition());
            }
        } else if (isEquals('-')) {
            if (Character.isDigit(currentChar)) {
                result = parseConst("-");
            } else {
                result = new Negate<>(number(), operations);
            }
        } else if (Character.isDigit(currentChar)) {
            result = parseConst("");
        } else if (Character.isLetter(currentChar)) {
            String var = parseStringVal();
            if (var.equals("count")) {
                result = new Count<>(number(), operations);
            } else {
                if (!isLegalVariableName(var) ||
                        (!Character.isWhitespace(currentChar)
                                && !allowedChars.contains(currentChar))) {
                    throw new VariableNameException("Illegal variable name", source.getCurrentPosition());
                }
                result = new Variable<>(var);
            }
        } else {
            throw new ArgumentException("No argument", source.getCurrentPosition());
        }

        skipWhiteSpace();
        return result;
    }

}
