/**
 * Взаимное исключение.
 */
interface Lock<N> {
    /**
     * Заходим в критическую секцию, возвращает ссылку на узел, который потом будет передан в [unlock].
     */
    fun lock(): N

    /**
     * Выходим из критической секции. Назад получим узел из [lock].
     */
    fun unlock(node: N)
}

inline fun <N> Lock<N>.withLock(block: () -> Unit) {
    val node = lock()
    block()
    unlock(node)
}