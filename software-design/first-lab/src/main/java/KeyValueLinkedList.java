public class KeyValueLinkedList<Key, Value> {

    private int size;

    private AbstractKeyValueNode<Key, Value> head;
    private AbstractKeyValueNode<Key, Value> last;


    public void sendFront(final AbstractKeyValueNode<Key, Value> node) {
        if (node.getPrev() == null) {
            return;
        }

        var prevNode = node.getPrev();
        var nextNode = node.getNext();
        var isTail = nextNode == null;
        AbstractKeyValueNode.setLinks(node.getPrev(), node.getNext());
        size--;
        prepend(node);
        if (isTail) {
            last = prevNode;
        }
    }

    public void prepend(final AbstractKeyValueNode<Key, Value> node) {
        AbstractKeyValueNode.setLinks(node, head);
        if (size == 1) {
            last = head;
        }
        head = node;
        size++;
    }

    public void dequeFront() {
        assert size > 0;
        head = head.getNext();
        if (head != null) {
            head.setPrev(null);
        }
        size--;
    }

    public void dequeLast() {
        assert size > 0;
        if (size == 1) {
            dequeFront();
            return;
        }
        last = last.getPrev();
        last.setNext(null);
        size--;
    }

    public void remove(final AbstractKeyValueNode<Key, Value> node) {
        if (node == head) {
            dequeFront();
        } else if (node == last) {
            dequeLast();
        } else {
            AbstractKeyValueNode.setLinks(node.getPrev(), node.getNext());
            size--;
        }
    }

    public int getSize() {
        return size;
    }

    public AbstractKeyValueNode<Key, Value> getLast() {
        return last;
    }

    public AbstractKeyValueNode<Key, Value> getHead() {
        return head;
    }

    public void setHead(final AbstractKeyValueNode<Key, Value> head) {
        this.head = head;
    }

    public void setLast(final AbstractKeyValueNode<Key, Value> last) {
        this.last = last;
    }
}
