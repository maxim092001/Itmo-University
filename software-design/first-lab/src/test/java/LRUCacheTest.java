import org.junit.Assert;
import org.junit.Test;

import java.util.Map;
import java.util.TreeMap;

public class LRUCacheTest {

    private static final int CAPACITY = 4;

    private static final Map<Integer, String> keyValues = new TreeMap<>(Map.of(
            1, "1",
            2, "2",
            3, "3",
            4, "4",
            5, "5"
    ));

    @Test
    public void testAddOneElement() {
        final var cache = new LRUCache<Integer, String>(CAPACITY);
        cache.put(1, keyValues.get(1));
        Assert.assertTrue(cache.containsKey(1));
        Assert.assertEquals(1, cache.size());
    }

    @Test
    public void testAddElementsMoreThanCapacity() {
        final var cache = new LRUCache<Integer, String>(CAPACITY);
        keyValues.forEach(cache::put);
        Assert.assertTrue(cache.containsKey(2));
        Assert.assertTrue(cache.containsKey(4));
        Assert.assertTrue(cache.containsKey(3));
        Assert.assertFalse(cache.containsKey(1));
        Assert.assertEquals(4, cache.size());
    }

    @Test(expected = AssertionError.class)
    public void testAddElementsMoreThanCapacityUsingPutAll() {
        final var cache = new LRUCache<Integer, String>(CAPACITY);
        cache.putAll(keyValues);
    }

    @Test
    public void testAddElementsAndGet() {
        final var cache = new LRUCache<Integer, String>(CAPACITY);
        cache.put(1, keyValues.get(1));
        cache.put(2, keyValues.get(2));
        cache.put(3, keyValues.get(3));
        cache.put(4, keyValues.get(4));
        Assert.assertEquals("1", cache.get(1));
        cache.put(5, keyValues.get(5));

        Assert.assertFalse(cache.containsKey(2));
        Assert.assertTrue(cache.containsKey(1));
        Assert.assertTrue(cache.containsKey(4));
        Assert.assertTrue(cache.containsKey(3));
        Assert.assertTrue(cache.containsKey(5));
        Assert.assertEquals(4, cache.size());
    }

    @Test(expected = AssertionError.class)
    public void testContainsNullKey() {
        new LRUCache<>(CAPACITY).containsKey(null);
    }

    @Test(expected = AssertionError.class)
    public void testContainsNullValue() {
        new LRUCache<>(CAPACITY).containsValue(null);
    }

    @Test
    public void testRemoveElement() {
        var cache = new LRUCache<>(CAPACITY);
        cache.put(1, 2);
        cache.get(1);
        cache.remove(1);
        Assert.assertFalse(cache.containsKey(1));
    }
}
