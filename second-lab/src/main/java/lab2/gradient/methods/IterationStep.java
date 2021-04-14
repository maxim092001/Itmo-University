package lab2.gradient.methods;

import lab2.gradient.utils.Vector;

public class IterationStep {
    private final long stepNumber;
    private final Vector vector;

    public IterationStep(long stepNumber, Vector vector) {
        this.stepNumber = stepNumber;
        this.vector = vector;
    }

    @Override
    public String toString() {
        return "IterationStep{" +
                "stepNumber=" + stepNumber +
                ", vector=" + vector +
                '}';
    }
}
