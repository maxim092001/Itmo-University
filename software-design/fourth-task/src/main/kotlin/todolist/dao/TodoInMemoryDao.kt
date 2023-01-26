package todolist.dao

import todolist.domain.TodoItem
import todolist.domain.TodoItemStatus
import todolist.domain.TodoList
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong
import mu.KotlinLogging

class TodoInMemoryDao : TodoDao {

    private val maxId = AtomicLong(0)
    private val maxTodoListId = AtomicLong(0)
    private val todoLists = ConcurrentHashMap<Long, TodoList>()
    private val logger = KotlinLogging.logger("TodoInMemoryDao")

    override fun addTodoList(name: String) {
        val id = maxTodoListId.incrementAndGet()
        val todoList = TodoList(id, name, mutableMapOf())
        todoLists[id] = todoList
    }

    override fun addTodo(todoListId: Long, name: String) {

        if (!todoLists.containsKey(todoListId)) {
            return
        }

        val id = maxId.incrementAndGet()
        val todoList = todoLists[todoListId]!!.todos
        todoList[id] = TodoItem(id, name, TodoItemStatus.NonResolved)
        logger.info { "Added todo {$name} into: {$todoListId}" }
    }

    override fun removeTodo(todoListId: Long, todoId: Long) {
        todoLists[todoListId]?.todos?.remove(todoId)
    }

    override fun removeTodoList(todoListId: Long) {
        todoLists.remove(todoListId)
    }

    override fun toggleTodo(todoListId: Long, todoId: Long) {
        val todoItem = todoLists[todoListId]?.todos?.get(todoId)
        if (todoItem == null) {
            logger.warn { "No such todo: {$todoId} in list: {$todoListId}" }
            return
        }
        todoLists[todoListId]!!.todos[todoId] = todoItem.copy(status = todoItem.status.swap())
        logger.info { "Changed todo status to resolved {$todoId} in list: {$todoListId}" }
    }

    override fun getAllTodoLists(): List<TodoList> = todoLists.values.toList()

    override fun getAllTodosByTodoListId(todoListId: Long): List<TodoItem> =
        todoLists[todoListId]?.todos?.values?.toList() ?: listOf()


}