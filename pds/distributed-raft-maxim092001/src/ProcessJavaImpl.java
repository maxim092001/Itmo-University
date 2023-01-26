package raft;

import org.jetbrains.annotations.NotNull;

/**
 * Raft algorithm implementation.
 * All functions are called from the single main thread.
 *
 * @author <First-Name> <Last-Name> // todo: replace with your name
 */
public class ProcessJavaImpl implements Process {
    private final Environment env;
    private final Storage storage;
    private final StateMachine machine;

    public ProcessJavaImpl(Environment env) {
        this.env = env;
        this.storage = env.getStorage();
        this.machine = env.getMachine();
    }

    @Override
    public void onTimeout() {
        /* todo: write implementation here */
    }

    @Override
    public void onMessage(int srcId, @NotNull Message message) {
        /* todo: write implementation here */
    }

    @Override
    public void onClientCommand(@NotNull Command command) {
        /* todo: write implementation here */
    }
}
