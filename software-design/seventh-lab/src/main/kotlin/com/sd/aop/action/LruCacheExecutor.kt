package com.sd.aop.action

import com.sd.aop.cache.LruCache

class LruCacheActionExecutor {
    private fun executeAction(cache: LruCache<Int, Int>, action: LruCacheAction) {
        when (action) {
            is Get -> cache[action.key]
            is Put -> cache[action.key] = action.value
            is IsEmpty -> cache.isEmpty()
            is Size -> cache.size()
            is Contains -> cache.contains(action.key)
        }
    }

    fun executeActions(cache: LruCache<Int, Int>, actions: List<LruCacheAction>) {
        actions.forEach { executeAction(cache, it) }
    }
}