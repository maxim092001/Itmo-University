package lab2.gradient.utils;

public class MinPointAndFunction {
    private Vector point;
    private double functionResult;

    private MinPointAndFunction(final Vector point, final double functionResult) {
        this.point = point;
        this.functionResult = functionResult;
    }

    public static MinPointAndFunction of(final Vector point, final double functionResult) {
        return new MinPointAndFunction(point, functionResult);
    }

    @Override
    public String toString() {
        return String.format("x = %s; f* = %f", point.toString(), functionResult);
    }

    public Vector getPoint() {
        return point;
    }

    public void setPoint(final Vector point) {
        this.point = point;
    }

    public double getFunctionResult() {
        return functionResult;
    }

    public void setFunctionResult(final double functionResult) {
        this.functionResult = functionResult;
    }
}
