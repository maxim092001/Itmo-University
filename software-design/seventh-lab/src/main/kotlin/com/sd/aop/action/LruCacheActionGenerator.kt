package com.sd.aop.action

import java.util.*

class LruCacheActionGenerator {
    private val random = Random()

    fun generateActions(n: Int): List<LruCacheAction> {
        return (0 until n).map {
            when ((0..4).random()) {
                0 -> {
                    val k = random.nextInt()
                    val v = random.nextInt()
                    Put(k, v)
                }

                1 -> {
                    val k = random.nextInt()
                    Get(k)
                }

                2 -> {
                    val k = random.nextInt()
                    Contains(k)
                }

                3 -> IsEmpty
                4 -> Size
                else -> throw IllegalStateException("Impossible")
            }
        }
    }
}