import java.util.concurrent.atomic.*;

public class SolutionTemplateJava implements Lock<SolutionTemplateJava.Node> {
    private final Environment env;

    // todo: необходимые поля (final, используем AtomicReference)

    public SolutionTemplateJava(Environment env) {
        this.env = env;
    }

    @Override
    public Node lock() {
        Node my = new Node(); // сделали узел
        // todo: алгоритм
        return my; // вернули узел
    }

    @Override
    public void unlock(Node node) {
        // todo: алгоритм
    }

    static class Node {
        final Thread thread = Thread.currentThread(); // запоминаем поток, которые создал узел
        // todo: необходимые поля (final, используем AtomicReference)
    }
}
