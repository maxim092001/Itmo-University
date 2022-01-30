/**
 * @author Grankin Maxim
 */
public class Solution implements AtomicCounter {

    private static class Node {
        private final int value;
        private final Consensus<Node> next = new Consensus<>();

        public Node(final int value) {
            this.value = value;
        }
    }

    private final ThreadLocal<Integer> current;
    private final ThreadLocal<Node> last;

    public Solution() {
        final Node node = new Node(0);
        current = ThreadLocal.withInitial(() -> 0);
        last = ThreadLocal.withInitial(() -> node);
    }

    @Override
    public int getAndAdd(final int x) {
        final Node node = new Node(x);
        int ans = 0;

        while (!last.get().equals(node)) {
            last.set(last.get().next.decide(node));
            ans = current.get();
            current.set(current.get() + last.get().value);
        }
        return ans;
    }
}
