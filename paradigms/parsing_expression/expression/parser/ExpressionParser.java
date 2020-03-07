package expression.parser;

import exception.ArgumentException;
import exception.ConstFormatException;
import exception.FormatException;
import exception.IllegalOperation;
import exception.ParenthesisException;
import exception.ParsingException;
import exception.VariableNameException;
import expression.CheckedAdd;
import expression.CheckedDivide;
import expression.CheckedLog;
import expression.CheckedMultiply;
import expression.CheckedNegate;
import expression.CheckedPow;
import expression.CheckedSubtract;
import expression.CommonExpression;
import expression.Const;
import expression.TripleExpression;
import expression.Variable;

import java.util.Set;

public class ExpressionParser implements Parser {

    private ExpressionSource source;
    private char currentChar;
    private Set<Character> allowedChars = Set.of(
            '+',
            '-',
            '*',
            '/',
            ')',
            '(',
            '\0'
    );

    private Set<String> allowVariables = Set.of(
            "x",
            "y",
            "z"
    );

    private void nextChar() {
        if (source.hasNext()) {
            currentChar = source.next();
        } else {
            currentChar = '\0';
        }
    }

    private boolean isEquals(char x) throws ParenthesisException {
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

    private void skipWhiteSpace() throws ParenthesisException {
        while (Character.isWhitespace(currentChar)) {
            nextChar();
        }
    }

    private Const parseConst(String sourcePrefix) throws ParsingException {
        StringBuilder number = new StringBuilder(sourcePrefix);
        while (Character.isDigit(currentChar)) {
            number.append(currentChar);
            nextChar();
        }
        skipWhiteSpace();
        try {
            return new Const(Integer.parseInt(number.toString()));
        } catch (NumberFormatException e) {
            throw new ConstFormatException("Illegal const format at ", source.getCurrentPosition());
        }
    }

    private String parseStringVal() throws ParsingException {
        skipWhiteSpace();
        StringBuilder str = new StringBuilder();
        while (Character.isLetter(currentChar)) {
            str.append(currentChar);
            nextChar();
        }
        return str.toString();
    }

    @Override
    public TripleExpression parse(String expression) throws ParsingException {
        source = new StringSource(expression);
        nextChar();
        skipWhiteSpace();
        CommonExpression result = addSub();
        skipWhiteSpace();
        if (source.hasNext()) {
            throw new FormatException("Space after: " + result.toString(), source.getCurrentPosition());
        }
        return result;
    }


    private CommonExpression addSub() throws ParsingException {
        CommonExpression result = mulDiv();
        skipWhiteSpace();
        while (true) {
            if (isEquals('+')) {
                result = new CheckedAdd(result, mulDiv());
            } else if (isEquals('-')) {
                result = new CheckedSubtract(result, mulDiv());
            } else {
                return result;
            }
        }
    }

    private CommonExpression powLog() throws ParsingException {
        CommonExpression result = number();
        skipWhiteSpace();
        while (true) {
            if (source.peekNext() == '*' && isEquals(source.peekNext())) {
                nextChar();
                result = new CheckedPow(result, number());
            } else if (source.peekNext() == '/' && isEquals(source.peekNext())) {
                nextChar();
                result = new CheckedLog(result, number());
            } else {
                return result;
            }
        }
    }

    private CommonExpression mulDiv() throws ParsingException {
        CommonExpression result = powLog();
        skipWhiteSpace();
        while (true) {
            if (isEquals('*')) {
                result = new CheckedMultiply(result, powLog());
            } else if (isEquals('/')) {
                result = new CheckedDivide(result, powLog());
            } else {
                return result;
            }
        }
    }


    private CommonExpression number() throws ParsingException {
        CommonExpression result;
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
                result = new CheckedNegate(number());
            }
        } else if (Character.isDigit(currentChar)) {
            result = parseConst("");
        } else if (Character.isLetter(currentChar)) {
            String var = parseStringVal();
            if (!isLegalVariableName(var) ||
                    (!Character.isWhitespace(currentChar)
                            && !allowedChars.contains(currentChar))) {
                throw new VariableNameException("Illegal variable name", source.getCurrentPosition());
            }
            result = new Variable(var);
        } else {
            throw new ArgumentException("No argument", source.getCurrentPosition());
        }

        skipWhiteSpace();
        return result;
    }

}
