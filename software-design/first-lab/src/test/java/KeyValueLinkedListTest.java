import org.junit.Assert;
import org.junit.Test;

public class KeyValueLinkedListTest {

    private final AbstractKeyValueNode<Integer, String> firstNode = new KeyValueNode<>(1, "1");
    private final AbstractKeyValueNode<Integer, String> secondNode = new KeyValueNode<>(2, "2");
    private final AbstractKeyValueNode<Integer, String> thirdNode = new KeyValueNode<>(3, "3");
    private final AbstractKeyValueNode<Integer, String> fourthNode = new KeyValueNode<>(4, "4");

    @Test
    public void testAddEements() {
        var list = new KeyValueLinkedList<Integer, String>();
        list.prepend(firstNode);
        list.prepend(secondNode);
        list.prepend(thirdNode);
        list.prepend(fourthNode);
        Assert.assertEquals("1", list.getLast().getValue());
        Assert.assertEquals("2", list.getLast().getPrev().getValue());
        Assert.assertEquals("3", list.getHead().getNext().getValue());
        Assert.assertEquals("4", list.getHead().getValue());
        Assert.assertEquals(4, list.getSize());
    }

    @Test
    public void testSendFront() {
        var list = new KeyValueLinkedList<Integer, String>();
        list.prepend(firstNode);
        list.prepend(thirdNode);
        list.sendFront(firstNode);
        Assert.assertEquals("3", list.getLast().getValue());
        Assert.assertEquals("1", list.getHead().getValue());
        Assert.assertEquals(2, list.getSize());
    }
}
