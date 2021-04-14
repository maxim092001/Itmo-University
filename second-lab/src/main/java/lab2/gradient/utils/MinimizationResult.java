package lab2.gradient.utils;

import lab2.gradient.methods.IterationStep;

import java.util.List;

public class MinimizationResult {
    private final Vector point;
    private final double functionResult;
    private final long numberOfIterations;
    private final List<IterationStep> steps;

    private MinimizationResult(final Vector point, final double functionResult, final long numberOfIterations, final List<IterationStep> steps) {
        this.point = point;
        this.functionResult = functionResult;
        this.numberOfIterations = numberOfIterations;
        this.steps = steps;
    }

    public static MinimizationResult of(final Vector point, final double functionResult, final long numberOfIterations, final List<IterationStep> steps) {
        return new MinimizationResult(point, functionResult, numberOfIterations, steps);
    }

    @Override
    public String toString() {
        return String.format("x = %s; f* = %f, number of iterations = %d", point.toString(), functionResult, numberOfIterations);
    }

    public Vector getPoint() {
        return point;
    }

    public double getFunctionResult() {
        return functionResult;
    }

    public List<IterationStep> getSteps() {
        return steps;
    }
}
