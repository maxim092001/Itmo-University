package queue;

public class Node {
    private Object value;
    private Node next;

    public Node(Object value, Node next) {
        this.value = value;
        this.next = next;
    }


    public Object getValue() {
        return value;
    }

    public Node getNext() {
        return next;
    }

    public void setNext(Node next) {
        this.next = next;
    }
}
