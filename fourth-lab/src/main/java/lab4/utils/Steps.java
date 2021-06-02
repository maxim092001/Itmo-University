package lab4.utils;

import lab4.matrix.Vector;

import java.util.List;

public class Steps {
    private final List<IterationStep> steps;
    private final Vector startPoint;

    public Steps(final List<IterationStep> steps, final Vector startPoint) {
        this.steps = steps;
        this.startPoint = startPoint;
    }

    public void addIteration(final double alpha, final Vector x, final Vector pk, final double fX) {
        steps.add(new IterationStep(alpha, x, pk, fX));
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder();
        sb.append("Start point: ").append(startPoint.view()).append(System.lineSeparator());
        for (IterationStep step : steps) {
            String str = String.format("%f %s %s %f%n", step.getAlpha(), step.getX().view(), step.getPk().view(), step.getfX());
            sb.append(str);
        }
        return sb.toString();
    }
}
