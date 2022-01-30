import java.util.concurrent.atomic.*

class Solution(private val env: Environment) : Lock<Solution.Node> {
    private val t = AtomicReference<Node>()

    override fun lock(): Node {
        val my = Node() // сделали узел
        my.locked.value = true
        val p = t.getAndSet(my)
        if (p != null) {
            p.nxt.set(my)
            while (my.locked.value) env.park()
        }
        return my // вернули узел
    }

    override fun unlock(node: Node) {
        when {
            node.nxt.get() == null && t.compareAndSet(node, null) -> return
            else -> while (node.nxt.get() == null) continue
        }
        node.nxt.get().locked.value = false
        env.unpark(node.nxt.get().thread)
    }

    class Node {
        val thread: Thread = Thread.currentThread() // запоминаем поток, которые создал узел
        val nxt = AtomicReference<Node>()
        val locked = AtomicReference(false)
    }
}