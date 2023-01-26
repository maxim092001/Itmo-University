package todolist.domain

sealed interface TodoItemStatus {

    object Resolved : TodoItemStatus

    object NonResolved : TodoItemStatus

    fun toBoolean(): Boolean = when (this) {
        Resolved -> true
        NonResolved -> false
    }

    fun swap(): TodoItemStatus = when(this) {
        Resolved -> NonResolved
        NonResolved -> Resolved
    }
}