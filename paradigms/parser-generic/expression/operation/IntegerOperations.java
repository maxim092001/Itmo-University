package expression.operation;

import expression.exception.DivisionByZeroException;
import expression.exception.OverflowException;

public class IntegerOperations implements Operations<Integer>{

    private final boolean checkedOperation;

    public IntegerOperations(boolean checkedOperation) {
        this.checkedOperation = checkedOperation;
    }

    @Override
    public Integer add(Integer a, Integer b) {
        if (checkedOperation) {
            checkAddOverflow(a, b);
        }
        return a + b;
    }

    @Override
    public Integer sub(Integer a, Integer b) {
        if (checkedOperation) {
            checkSubOnOverflow(a, b);
        }
        return a - b;
    }

    @Override
    public Integer div(Integer a, Integer b) {
        if (b == 0) {
            throw new DivisionByZeroException("Division by zero: " + a + "/" + b);
        }
        if (checkedOperation) {
            checkDivideOnOverflow(a, b);
        }
        return a / b;
    }

    @Override
    public Integer mul(Integer a, Integer b) {
        if (checkedOperation) {
            checkMultiplyOnOverflow(a, b);
        }
        return a * b;
    }

    @Override
    public Integer min(Integer a, Integer b) {
        return Integer.min(a, b);
    }

    @Override
    public Integer max(Integer a, Integer b) {
        return Integer.max(a, b);
    }

    @Override
    public Integer parseValue(String a) {
        return Integer.parseInt(a);
    }

    @Override
    public Integer negate(Integer a) {
        if (checkedOperation) {
            checkNegateOverflow(a);
        }
        return -a;
    }

    @Override
    public Integer count(Integer a) {
        return Integer.bitCount(a);
    }

    private void checkNegateOverflow(Integer a) {
        if (a == Integer.MIN_VALUE) {
            throw new OverflowException("Negation overflow: " + a);
        }
    }

    private void checkAddOverflow(Integer a, Integer b) {
        if ((b > 0 && a > Integer.MAX_VALUE - b) || (b < 0 && a < Integer.MIN_VALUE - b)) {
            throw new OverflowException("Addition overflow: " + a + "+" + b);
        }
    }

    private void checkSubOnOverflow(Integer a, Integer b) {
        if ((b > 0 && a < Integer.MIN_VALUE + b) || (b < 0 && a > Integer.MAX_VALUE + b)) {
            throw new OverflowException("Subtraction overflow: " + a + "-" + b);
        }
    }

    private void checkDivideOnOverflow(Integer a, Integer b) {
        if (a == Integer.MIN_VALUE && b == -1) {
            throw new OverflowException("Division overflow: " + a + "/" + b);
        }
    }

    private void checkMultiplyOnOverflow(Integer a, Integer b) {
        int maximum = Integer.signum(a) == Integer.signum(b) ? Integer.MAX_VALUE : Integer.MIN_VALUE;

        if ((a == -1 && b == Integer.MIN_VALUE)
                || (a != -1 && a != 0 && ((b > 0 && b > maximum / a)
                || (b < 0 && b < maximum / a )))) {
            throw new OverflowException("Multiplication overflow: " + a + "*" + b);
        }
    }
}
