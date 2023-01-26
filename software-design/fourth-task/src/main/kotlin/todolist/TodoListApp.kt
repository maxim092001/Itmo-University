package todolist

import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.runApplication

@SpringBootApplication
open class TodoListApp


fun main(args: Array<String>) {
    runApplication<TodoListApp>(*args)
}