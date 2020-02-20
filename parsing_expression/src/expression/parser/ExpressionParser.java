package expression.parser;

import exception.ArgumentException;
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

import java.util.HashSet;
import java.util.Set;

public class ExpressionParser implements Parser {

    private ExpressionSource source;
    private char currentChar;
    private int balance;
    private Set<Character> allowedChars = new HashSet<>();
    private Set<String> allowVariables = new HashSet<>();

    {
        allowedChars.add('+');
        allowedChars.add('-');
        allowedChars.add('*');
        allowedChars.add('/');
        allowedChars.add(')');
        allowedChars.add('(');
        allowVariables.add("x");
        allowVariables.add("y");
        allowVariables.add("z");
    }

    private void nextChar() throws ParenthesisException {
        if (source.hasNext()) {
            currentChar = source.next();
            if (currentChar == '(') {
                balance++;
            } else if (currentChar == ')') {
                balance--;
                if (balance < 0) {
                    throw new ParenthesisException("Mismatched parenthesis", source.getCurrentPosition());
                }
            }
        } else {
            currentChar = '\0';
        }
    }

    private boolean isEquals(char first, char second) throws ParenthesisException {
        skipWhiteSpace();
        if (first == second) {
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

    private Const parseConst(String sourcePrefix) throws ParenthesisException {
        StringBuilder number = new StringBuilder(sourcePrefix);
        while (Character.isDigit(currentChar)) {
            number.append(currentChar);
            nextChar();
        }
        return new Const(Integer.parseInt(number.toString()));
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
        balance = 0;
        nextChar();
        skipWhiteSpace();
        CommonExpression result = addSub();
        skipWhiteSpace();
        if (source.hasNext()) {
            throw new FormatException("Space after: " + result.toString(), source.getCurrentPosition());
        }
        if (balance > 0) {
            throw new ParenthesisException("Mismatched parenthesis", source.getCurrentPosition());
        }
        return result;
    }


    private CommonExpression addSub() throws ParsingException {
        CommonExpression result = mulDiv();
        skipWhiteSpace();
        while (true) {
            if (isEquals(currentChar, '+')) {
                result = new CheckedAdd(result, mulDiv());
            } else if (isEquals(currentChar, '-')) {
                result = new CheckedSubtract(result, mulDiv());
            } else if (!source.hasNext() || currentChar == ')') {
                return result;
            } else {
                throw new IllegalOperation("Illegal Operation", source.getCurrentPosition());

            }
        }
    }

    private CommonExpression powLog() throws ParsingException {
        CommonExpression result = number();
        skipWhiteSpace();
        while (true) {
            if (source.peekNext() == '*' && isEquals(currentChar, source.peekNext())) {
                nextChar();
                result = new CheckedPow(result, number());
            } else if (source.peekNext() == '/' && isEquals(currentChar, source.peekNext())) {
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
            if (isEquals(currentChar, '*')) {
                result = new CheckedMultiply(result, powLog());
            } else if (isEquals(currentChar, '/')) {
                result = new CheckedDivide(result, powLog());
            } else {
                return result;
            }
        }
    }


    private CommonExpression number() throws ParsingException {
        CommonExpression result;
        skipWhiteSpace();
        if (isEquals(currentChar, '(')) {
            result = addSub();
            nextChar();
        } else if (isEquals(currentChar, '-')) {
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
                    (currentChar != '\0' && !Character.isWhitespace(currentChar)
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

