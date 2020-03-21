package expression.generic;

import expression.exception.EvaluatingException;
import expression.exception.IncorrectModeException;
import expression.exception.ParsingException;
import expression.operation.BigIntegerOperations;
import expression.operation.DoubleOperations;
import expression.operation.IntegerOperations;
import expression.operation.LongOperations;
import expression.operation.Operations;
import expression.operation.ShortOperations;
import expression.parser.ExpressionParser;
import expression.parser.GenericTripleExpression;

import java.util.Map;

public class GenericTabulator implements Tabulator {

    private Map<String, Operations<?>> modes = Map.of(
            "i", new IntegerOperations(true),
            "u", new IntegerOperations(false),
            "d", new DoubleOperations(),
            "bi", new BigIntegerOperations(),
            "s", new ShortOperations(),
            "l", new LongOperations()
    );


    @Override
    public Object[][][] tabulate(String mode, String expression, int x1, int x2, int y1, int y2, int z1, int z2) throws Exception {
        if (modes.containsKey(mode)) {
            return fillMatrix(expression, x1,  x2, y1, y2, z1, z2, modes.get(mode));
        } else {
            throw new IncorrectModeException(mode);
        }
    }

    private <T> Object[][][] fillMatrix(String expression, int x1, int x2, int y1, int y2, int z1, int z2,
                                        Operations<T> operation) {
        ExpressionParser<T> parser = new ExpressionParser<>(operation);
        Object[][][] result = new Object[x2 - x1 + 1][y2 - y1 + 1][z2 - z1 + 1];

        GenericTripleExpression<T> tripleExpression;
        try {
            tripleExpression = parser.parse(expression);
        } catch (ParsingException e) {
            return null;
        }

        for (int x = x1; x <= x2; x++) {
            for (int y = y1; y <= y2; y++) {
                for (int z = z1; z <= z2; z++) {
                    T i = operation.parseValue(Integer.toString(x));
                    T j = operation.parseValue(Integer.toString(y));
                    T k = operation.parseValue(Integer.toString(z));

                    try {
                        result[x - x1][y - y1][z - z1] = tripleExpression.evaluate(i, j, k);
                    } catch (EvaluatingException e) {
                        result[x - x1][y - y1][z - z1] = null;
                    }
                }
            }
        }

        return result;

    }
}
