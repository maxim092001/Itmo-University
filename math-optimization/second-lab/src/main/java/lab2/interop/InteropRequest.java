package lab2.interop;

import lab2.gradient.utils.QuadraticFunction;
import lab2.gradient.utils.Vector;
import lombok.Data;

@Data
public class InteropRequest {
    private String method;
    private double[] function;
    private double[] startPoint;
    private double eps, alpha;

    public QuadraticFunction getParsedFunction() {
        return QuadraticFunction.from2d(function[0], function[1], function[2], function[3], function[4], function[5]);
    }

    public Vector getParsedStartPoint() {
        return new Vector(startPoint);
    }
}
