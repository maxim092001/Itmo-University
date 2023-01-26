package todolist.dto

data class TodoListDto(val id: Long?, val name: String?, val todos: List<TodoItemDto>)
