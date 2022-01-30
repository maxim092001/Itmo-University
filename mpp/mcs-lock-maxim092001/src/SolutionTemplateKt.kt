import java.util.concurrent.atomic.*

class SolutionTemplateKt(val env: Environment) : Lock<SolutionTemplateKt.Node> {
    // todo: необходимые поля (val, используем AtomicReference)

    override fun lock(): Node {
        val my = Node() // сделали узел
        // todo: алгоритм
        return my // вернули узел
    }

    override fun unlock(node: Node) {
        // todo: алгоритм
    }

    class Node {
        val thread = Thread.currentThread() // запоминаем поток, которые создал узел
        // todo: необходимые поля (val, используем AtomicReference)
    }
}