package msqueue;

import org.jetbrains.kotlinx.lincheck.verifier.VerifierState;

import java.util.ArrayDeque;

public class SequentialQueue extends VerifierState implements Queue {
    private final ArrayDeque<Integer> q = new ArrayDeque<>();

    @Override
    public void enqueue(int x) {
        q.add(x);
    }

    @Override
    public int dequeue() {
        if (q.isEmpty()) return Integer.MIN_VALUE;
        else return q.poll();
    }

    @Override
    public int peek() {
        if (q.isEmpty()) return Integer.MIN_VALUE;
        else return q.peek();
    }

    @Override
    protected Object extractState() {
        return q.toString();
    }
}
