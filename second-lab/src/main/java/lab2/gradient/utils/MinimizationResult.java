package lab2.gradient.utils;

public class MinimizationResult {
    private final Vector point;
    private final double functionResult;
    private final long numberOfIterations;

    private MinimizationResult(final Vector point, final double functionResult, final long numberOfIterations) {
        this.point = point;
        this.functionResult = functionResult;
        this.numberOfIterations = numberOfIterations;
    }

    public static MinimizationResult of(final Vector point, final double functionResult, final long numberOfIterations) {
        return new MinimizationResult(point, functionResult, numberOfIterations);
    }

    @Override
    public String toString() {
        return String.format("x = %s; f* = %f, number of iterations = %d", point.toString(), functionResult, numberOfIterations + 1);
    }

    public Vector getPoint() {
        return point;
    }

    public double getFunctionResult() {
        return functionResult;
    }

}
