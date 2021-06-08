package lab4.utils;

import lab4.matrix.Vector;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

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

    public int size() {
        return steps.size();
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder();
        sb.append("Start point: ").append(startPoint.texView()).append(System.lineSeparator());
        sb.append(steps.size()).append(System.lineSeparator());
        for (int i = 0, stepsSize = steps.size(); i < stepsSize; i++) {
            final IterationStep step = steps.get(i);
            String str = String.format("%d & %.7f & %s & %s & %.7f\\\\%n\\hline%n", i, step.getAlpha(), step.getX().texView(), step.getPk().texView(), step.getfX());
            sb.append(str);
        }
        return sb.toString();
    }

    public String toWolfram(final Function<Vector, Double> f, Vector startPoint) {
        List<Vector> list = steps.stream().map(IterationStep::getX).collect(Collectors.toCollection(ArrayList::new));
        list.add(0, startPoint);
        return "{" +
                list.stream().map(i -> i.wolframView(f)).collect(Collectors.joining(",")) +
                "}";
    }
}
