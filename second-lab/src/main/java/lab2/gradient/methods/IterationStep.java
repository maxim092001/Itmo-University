package lab2.gradient.methods;

import lab2.gradient.utils.Vector;

public class IterationStep {
    private final long stepNumber;
    private final Vector vector;
    private final double functionResult;

    public IterationStep(final long stepNumber, final Vector vector, final double functionResult) {
        this.stepNumber = stepNumber;
        this.vector = vector;
        this.functionResult = functionResult;
    }

    @Override
    public String toString() {
        var s =  String.format("\\hline \n %d & %f & %s", stepNumber, functionResult, vector);
        return s.substring(0, s.length() - 2) + "\\\\";
    }
}
