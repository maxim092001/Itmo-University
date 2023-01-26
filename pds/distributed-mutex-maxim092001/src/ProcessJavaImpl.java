package mutex;

import org.jetbrains.annotations.NotNull;

/**
 * Distributed mutual exclusion implementation.
 * All functions are called from the single main thread.
 *
 * @author <First-Name> <Last-Name> // todo: replace with your name
 */
public class ProcessJavaImpl implements Process {
    private final Environment env;

    public ProcessJavaImpl(Environment env) {
        this.env = env;
    }

    @Override
    public void onMessage(int srcId, @NotNull Message message) {
        /* todo: write implementation here */
    }

    @Override
    public void onLockRequest() {
        /* todo: write implementation here */
        env.locked();
    }

    @Override
    public void onUnlockRequest() {
        /* todo: write implementation here */
        env.unlocked();
    }
}
