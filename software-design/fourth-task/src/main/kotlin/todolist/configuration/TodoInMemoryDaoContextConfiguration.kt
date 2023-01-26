package todolist.configuration

import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration
import todolist.dao.TodoDao
import todolist.dao.TodoInMemoryDao

@Configuration
open class TodoInMemoryDaoContextConfiguration {

    @Bean
    open fun todoDao(): TodoDao {
        return TodoInMemoryDao()
    }
}