package stack;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.kotlinx.lincheck.verifier.VerifierState;

public class SequentialStack extends VerifierState implements Stack {
    private java.util.Stack<Integer> s = new java.util.Stack<>();

    @Override
    public void push(int x) {
        s.push(x);
    }

    @Override
    public int pop() {
        if (s.isEmpty()) return Integer.MIN_VALUE;
        else return s.pop();
    }

    @NotNull
    @Override
    protected Object extractState() {
        return s;
    }
}