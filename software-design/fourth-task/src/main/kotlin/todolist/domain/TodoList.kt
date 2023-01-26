package todolist.domain

data class TodoList(val id: Long, val name: String, val todos: MutableMap<Long, TodoItem>)