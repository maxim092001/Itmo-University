package stack;

public interface Stack {

    /**
     * Pushes the corresponding element onto the top of this stack.
     */
    void push(int x);

    /**
     * Removes the object at the top of this stack and returns that
     * object as the value of this function, or {@link Integer#MIN_VALUE}
     * if this stack is empty.
     */
    int pop();
}
