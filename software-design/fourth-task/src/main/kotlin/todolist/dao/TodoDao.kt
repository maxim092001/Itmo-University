package todolist.dao

import todolist.domain.TodoItem
import todolist.domain.TodoList

interface TodoDao {
    fun addTodoList(name: String)

    fun addTodo(todoListId: Long, name: String)

    fun removeTodo(todoListId: Long, todoId: Long)

    fun removeTodoList(todoListId: Long)

    fun toggleTodo(todoListId: Long, todoId: Long)

    fun getAllTodoLists(): List<TodoList>

    fun getAllTodosByTodoListId(todoListId: Long): List<TodoItem>
}