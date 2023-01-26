import java.util.Objects;

public abstract class AbstractKeyValueNode<Key, Value> {

    private AbstractKeyValueNode<Key, Value> prev;

    private AbstractKeyValueNode<Key, Value> next;

    private Value value;

    private final Key key;

    public AbstractKeyValueNode(final AbstractKeyValueNode<Key, Value> prev, final AbstractKeyValueNode<Key, Value> next, final Key key, final Value value) {
        this.prev = prev;
        this.next = next;
        this.key = key;
        this.value = value;
    }

    public void setPrev(final AbstractKeyValueNode<Key, Value> prev) {
        this.prev = prev;
    }

    public void setNext(final AbstractKeyValueNode<Key, Value> next) {
        this.next = next;
    }

    public AbstractKeyValueNode<Key, Value> getPrev() {
        return prev;
    }

    public AbstractKeyValueNode<Key, Value> getNext() {
        return next;
    }

    public Value getValue() {
        return value;
    }

    public void setValue(final Value value) {
        this.value = value;
    }

    public Key getKey() {
        return key;
    }

    public static <Key, Value> void setLinks(final AbstractKeyValueNode<Key, Value> first, final AbstractKeyValueNode<Key, Value> second) {
        if (Objects.nonNull(first)) {
            first.next = second;
        }
        if (Objects.nonNull(second)) {
            second.prev = first;
        }
    }
}
