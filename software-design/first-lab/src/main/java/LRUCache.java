import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public class LRUCache<Key, Value> implements Map<Key, Value> {

    private final int capacity;
    private final Map<Key, AbstractKeyValueNode<Key, Value>> keyNodeMap;
    private final KeyValueLinkedList<Key, Value> keyValueLinkedList = new KeyValueLinkedList<>();

    public LRUCache(final int capacity) {
        assert capacity > 0;
        this.capacity = capacity;
        keyNodeMap = new HashMap<>(capacity);
    }

    @Override
    public int size() {
        return keyNodeMap.size();
    }

    @Override
    public boolean isEmpty() {
        return keyNodeMap.isEmpty();
    }

    @Override
    public boolean containsKey(final Object key) {
        assert key != null;
        return keyNodeMap.containsKey(key);
    }

    @SuppressWarnings("unchecked")
    @Override
    public boolean containsValue(final Object value) {
        assert value != null;
        return values().contains((Value) value);
    }

    @SuppressWarnings("unchecked")
    @Override
    public Value get(final Object key) {
        if (!keyNodeMap.containsKey((Key) key)) {
            return null;
        }
        final var node = keyNodeMap.get(key);
        keyValueLinkedList.sendFront(node);
        return node.getValue();
    }

    @Override
    public Value put(final Key key, final Value value) {
        assert key != null;
        assert value != null;

        if (keyNodeMap.containsKey(key)) {
            final var node = keyNodeMap.get(key);
            keyValueLinkedList.sendFront(node);
            final var old = node.getValue();
            node.setValue(value);
            return old;
        }
        final var newNode = new KeyValueNode<>(key, value);
        if (capacity == size()) {
            final var lastNode = keyValueLinkedList.getLast();
            keyNodeMap.remove(lastNode.getKey());
            keyValueLinkedList.dequeLast();
        }
        keyValueLinkedList.prepend(newNode);
        keyNodeMap.put(key, newNode);
        return value;
    }

    @SuppressWarnings("unchecked")
    @Override
    public Value remove(final Object key) {
        if (!keyNodeMap.containsKey((Key) key)) {
            return null;
        }
        final var node = keyNodeMap.get((Key) key);
        keyNodeMap.remove(key);
        keyValueLinkedList.remove(node);
        return node.getValue();
    }

    @Override
    public void putAll(final Map<? extends Key, ? extends Value> m) {
        assert m.size() <= capacity;
        m.forEach(this::put);
    }

    @Override
    public void clear() {
        keyNodeMap.clear();
        keyValueLinkedList.setHead(null);
        keyValueLinkedList.setLast(null);
    }

    @Override
    public Set<Key> keySet() {
        return keyNodeMap.keySet();
    }

    @Override
    public Collection<Value> values() {
        return keyNodeMap.values().stream().map(AbstractKeyValueNode::getValue).collect(Collectors.toList());
    }

    @Override
    public Set<Entry<Key, Value>> entrySet() {
        return keyNodeMap.entrySet()
                .stream()
                .map(e -> Map.entry(e.getKey(), e.getValue().getValue()))
                .collect(Collectors.toSet());
    }
}
