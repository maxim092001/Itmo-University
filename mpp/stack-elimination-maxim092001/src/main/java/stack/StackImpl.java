package stack;

import kotlinx.atomicfu.AtomicRef;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class StackImpl implements Stack {

    // head pointer
    private final AtomicRef<Node> headRef = new AtomicRef<>(null);
    private final EliminationOptimizer eliminationOptimizer = new EliminationOptimizer();


    private static class Node {
        final AtomicRef<Node> next;
        final int x;

        Node(final int x, final Node next) {
            this.next = new AtomicRef<>(next);
            this.x = x;
        }
    }


    private static class EliminationOptimizer {
        private static final int ELIMINATION_SIZE = 8;
        private static final int ELIMINATION_STEP = ELIMINATION_SIZE / 4;
        private static final int ELIMINATION_ITERATIONS_WAIT = 4;
        private final Random rnd = new Random();
        private final List<AtomicRef<Integer>> eliminationArray = new ArrayList<>();


        public EliminationOptimizer() {
            for (int i = 0; i < ELIMINATION_SIZE; i++) {
                eliminationArray.add(new AtomicRef<Integer>(null));
            }
        }


        public boolean push(final int x) {
            final int randomEliminationIndex = rnd.nextInt(ELIMINATION_SIZE);
            final Integer elementReference = x;
            final AtomicRef<Integer> positionReference = writeElement(elementReference, randomEliminationIndex);

            if (positionReference == null) {
                return false;
            }

            for (int i = 0; i < ELIMINATION_ITERATIONS_WAIT; i++) {
                if (positionReference.compareAndSet(null, null)) {
                    return true;
                }
            }

            return !positionReference.compareAndSet(elementReference, null);
        }


        private AtomicRef<Integer> writeElement(
                final Integer elementReference,
                final int index
        ) {
            AtomicRef<Integer> positionReference;
            for (int step = 0; step < ELIMINATION_STEP; step++) {
                positionReference = getReferenceByIndex(index, step);
                if (positionReference.compareAndSet(null, elementReference)) {
                    return positionReference;
                }
            }
            return null;
        }

        public Integer pop() {
            final int randomEliminationIndex = rnd.nextInt(ELIMINATION_SIZE);

            AtomicRef<Integer> elementReference;
            for (int step = 0; step < ELIMINATION_STEP; step++) {
                elementReference = getReferenceByIndex(randomEliminationIndex, step);
                for (int i = 0; i < ELIMINATION_ITERATIONS_WAIT; i++) {
                    final Integer element = elementReference.getAndSet(null);
                    if (element != null) {
                        return element;
                    }
                }
            }

            return null;
        }


        private AtomicRef<Integer> getReferenceByIndex(final int index, final int step) {
            return eliminationArray.get((index + step) % ELIMINATION_SIZE);
        }
    }

    
    @Override
    public void push(final int x) {

        if (eliminationOptimizer.push(x)) {
            return;
        }

        while (true) {
            final Node head = headRef.getValue();
            final Node newHead = new Node(x, head);
            if (headRef.compareAndSet(head, newHead)) {
                return;
            }
        }
    }


    @Override
    public int pop() {

        final Integer eliminationElement = eliminationOptimizer.pop();

        if (eliminationElement != null) {
            return eliminationElement;
        }

        while (true) {
            final Node head = headRef.getValue();
            if (head == null) {
                return Integer.MIN_VALUE;
            }
            if (headRef.compareAndSet(head, head.next.getValue())) {
                return head.x;
            }
        }
    }
}
