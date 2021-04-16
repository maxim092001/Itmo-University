package lab2.gradient.methods;

import lab2.gradient.utils.Vector;

/**
 * Iteration step POJO.
 */
public class IterationStep {

    /**
     * Step number.
     */
    private final long stepNumber;

    /**
     * Vector (point).
     */
    private final Vector vector;

    /**
     * Function result in {@link IterationStep#vector}
     */
    private final double functionResult;

    public IterationStep(final long stepNumber, final Vector vector, final double functionResult) {
        this.stepNumber = stepNumber;
        this.vector = vector;
        this.functionResult = functionResult;
    }

    /**
     * Get step number.
     *
     * @return step number.
     */
    public long getStepNumber() {
        return stepNumber;
    }

    /**
     * Get vector.
     *
     * @return vector.
     */
    public Vector getVector() {
        return vector;
    }

    /**
     * Get function result in {@link IterationStep#vector}.
     *
     * @return function result.
     */
    public double getFunctionResult() {
        return functionResult;
    }


    @Override
    public String toString() {
        var s =  String.format("\\hline \n %d & %f & %s", stepNumber, functionResult, vector);
        return s.substring(0, s.length() - 2) + "\\\\";
    }
}
