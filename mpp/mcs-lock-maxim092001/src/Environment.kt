import java.util.concurrent.atomic.*

/**
 * Окружение в котором происходит тестирование.
 */
interface Environment {
    fun park()
    fun unpark(thread: Thread)
}

/**
 * Свойство для более идеоматичной работы с [AtomicReference] из Kotlin.
 */
var <T> AtomicReference<T>.value: T
    get() = get()
    set(value) = set(value)
