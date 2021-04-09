package lab2;

public class MinPointAndFunction {
    private MultiDimensionalPoint point;
    private double functionResult;

    private MinPointAndFunction(final MultiDimensionalPoint point, final double functionResult) {
        this.point = point;
        this.functionResult = functionResult;
    }

    public static MinPointAndFunction of(final MultiDimensionalPoint point, final double functionResult) {
        return new MinPointAndFunction(point, functionResult);
    }

    public MultiDimensionalPoint getPoint() {
        return point;
    }

    public void setPoint(final MultiDimensionalPoint point) {
        this.point = point;
    }

    public double getFunctionResult() {
        return functionResult;
    }

    public void setFunctionResult(final double functionResult) {
        this.functionResult = functionResult;
    }
}
