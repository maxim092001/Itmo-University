package com.sd.aop

import com.sd.aop.aspect.ProfilerAspect
import com.sd.aop.cache.LruCache
import com.sd.aop.cache.LruCacheImpl
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration
import org.springframework.context.annotation.EnableAspectJAutoProxy

@Configuration
@EnableAspectJAutoProxy
open class ContextConfiguration {
    @Bean
    open fun aspect(): ProfilerAspect {
        return ProfilerAspect()
    }

    @Bean
    open fun cache(): LruCache<Int, Int> {
        return LruCacheImpl(64)
    }
}