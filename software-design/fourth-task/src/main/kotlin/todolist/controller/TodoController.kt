package todolist.controller

import org.springframework.stereotype.Controller
import org.springframework.ui.ModelMap
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.web.bind.annotation.RequestMethod
import org.springframework.web.bind.annotation.RequestParam
import todolist.dao.TodoDao
import todolist.dto.TodoItemDto
import todolist.dto.TodoListDto

@Controller
class TodoController(private val todoDao: TodoDao) {

    @RequestMapping(value = ["/todos"], method = [RequestMethod.GET])
    fun getTodos(modelMap: ModelMap): String {
        prepareModelMap(modelMap, todoDao.getAllTodoLists().map {
            TodoListDto(it.id, it.name, it.todos.values.map { item -> TodoItemDto(item.id, item.name, item.status.toBoolean()) }.toList())
        })
        return "index"
    }

    @RequestMapping(value = ["/add-todo-list"], method = [RequestMethod.POST])
    fun addTodoList(@RequestParam("todolist_name") todoListName: String): String {
        todoDao.addTodoList(todoListName)
        return "redirect:/todos"
    }

    @RequestMapping(value = ["/add-todo"], method = [RequestMethod.POST])
    fun addTodo(
        @RequestParam("list_id") listId: Long,
        @RequestParam("todo_name") todoName: String
    ): String {
        todoDao.addTodo(listId, todoName)
        return "redirect:/todos"
    }

    @RequestMapping(value = ["/remove-todo"], method = [RequestMethod.POST])
    fun removeTodo(
        @RequestParam("list_id") listId: Long,
        @RequestParam("id") id: Long
    ): String {
        todoDao.removeTodo(listId, id)
        return "redirect:/todos"
    }

    @RequestMapping(value = ["/remove-todo-list"], method = [RequestMethod.POST])
    fun removeTodoList(@RequestParam("list_id") listId: Long): String {
        todoDao.removeTodoList(listId)
        return "redirect:/todos"
    }

    @RequestMapping(value = ["/toggle-todo"], method = [RequestMethod.POST])
    fun toggleTodo(@RequestParam("list_id") listId: Long, @RequestParam("id") id: Long): String {
        todoDao.toggleTodo(listId, id)
        return "redirect:/todos"
    }

    private fun prepareModelMap(modelMap: ModelMap, todoLists: List<TodoListDto>) {
        modelMap.apply {
            addAttribute("todolists", todoLists)
        }
    }
}