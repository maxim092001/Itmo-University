package com.sd.aop

import com.sd.aop.action.LruCacheActionExecutor
import com.sd.aop.action.LruCacheActionGenerator
import com.sd.aop.aspect.ProfilerAspect
import com.sd.aop.cache.LruCache
import org.springframework.context.ApplicationContext
import org.springframework.context.annotation.AnnotationConfigApplicationContext

fun main() {
    val ctx: ApplicationContext = AnnotationConfigApplicationContext(ContextConfiguration::class.java)
    val cache = ctx.getBean(LruCache::class.java) as LruCache<Int, Int>

    val actionsGenerator = LruCacheActionGenerator()
    val actionsExecutor = LruCacheActionExecutor()

    val actions = actionsGenerator.generateActions(10_000)
    actionsExecutor.executeActions(cache, actions)

    val profilingAspect = ctx.getBean(ProfilerAspect::class.java)
    profilingAspect.printExecutionTime()
}