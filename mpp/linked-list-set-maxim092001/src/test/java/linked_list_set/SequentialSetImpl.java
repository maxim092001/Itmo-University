package linked_list_set;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.kotlinx.lincheck.verifier.VerifierState;

import java.util.HashSet;

public class SequentialSetImpl extends VerifierState implements Set {
    private HashSet<Integer> set = new HashSet<>();

    @Override
    public boolean add(int x) {
        return set.add(x);
    }

    @Override
    public boolean remove(int x) {
        return set.remove(x);
    }

    @Override
    public boolean contains(int x) {
        return set.contains(x);
    }

    @NotNull
    @Override
    protected Object extractState() {
        return set;
    }
}
