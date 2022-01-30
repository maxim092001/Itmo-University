import java.util.concurrent.atomic.AtomicReference
import kotlin.coroutines.Continuation
import kotlin.coroutines.resume
import kotlin.coroutines.suspendCoroutine

sealed interface Node<T> {
    val value: AtomicReference<T?>
    var cont: Continuation<Boolean>?
    val nxt: AtomicReference<Node<T>>
}

class Send<T>(
    override val value: AtomicReference<T?>,
    override var cont: Continuation<Boolean>? = null,
    override val nxt: AtomicReference<Node<T>> = AtomicReference(null)
) : Node<T>

class Receive<T>(
    override val value: AtomicReference<T?>,
    override var cont: Continuation<Boolean>? = null,
    override val nxt: AtomicReference<Node<T>> = AtomicReference(null)
) : Node<T>

class None<T>(
    override val value: AtomicReference<T?> = AtomicReference(null),
    override var cont: Continuation<Boolean>? = null,
    override val nxt: AtomicReference<Node<T>> = AtomicReference(null)
) : Node<T>

class SynchronousQueueMS<E> : SynchronousQueue<E> {
    private val none: Node<E> = None()
    private val head: AtomicReference<Node<E>> = AtomicReference(none)
    private val tail: AtomicReference<Node<E>> = AtomicReference(none)

    private suspend fun suspendCoroutineWithNode(node: Node<E>, t: Node<E>) =
        suspendCoroutine<Boolean> { cont ->
            node.cont = cont
            when {
                t.nxt.compareAndSet(null, node) -> tail.compareAndSet(t, node)
                else -> cont.resume(false)
            }
        }

    override suspend fun send(element: E) {
        while (true) {
            val t = tail.get()
            val h = head.get()
            when {
                (h == t || t !is Receive<E>) && suspendCoroutineWithNode(
                    Send(AtomicReference(element)),
                    t
                ) -> return
                else -> if (t == tail.get() && h == head.get() && h != tail.get()) {
                    val nxt = h.nxt.get() ?: continue
                    if (casPredicate(nxt, h, nxt is Receive<E>)) {
                        nxt.value.compareAndSet(null, element)
                        nxt.cont!!.resume(true)
                        return
                    }
                }
            }
        }
    }

    override suspend fun receive(): E {
        while (true) {
            val t = tail.get()
            val h = head.get()
            val node: Node<E> = Receive(AtomicReference(null))

            when {
                (h == t || t is Receive<E>) && suspendCoroutineWithNode(
                    node,
                    t
                ) -> return node.value.get()!!
                else -> if (t == tail.get() && h != tail.get() && h == head.get()) {
                    val nxt = h.nxt.get() ?: continue
                    val element = nxt.value.get() ?: continue
                    if (casPredicate(nxt, h, nxt !is Receive<E>)) {
                        nxt.value.compareAndSet(element, null)
                        nxt.cont!!.resume(true)
                        return element
                    }
                }
            }
        }
    }

    private fun casPredicate(nxt: Node<E>, h: Node<E>, receive: Boolean): Boolean =
        nxt.cont !== null && receive && head.compareAndSet(h, nxt)
}