
package com.sd.aop.aspect

import de.vandermeer.asciitable.AsciiTable
import org.aspectj.lang.ProceedingJoinPoint
import org.aspectj.lang.annotation.Around
import org.aspectj.lang.annotation.Aspect

@Aspect
class ProfilerAspect {
    private val method2Executions: MutableMap<String, MutableList<Long>> = mutableMapOf()

    @Around("execution(* com.sd.aop.cache..*.*(..))")
    fun logMethodExecutionTime(jointPoint: ProceedingJoinPoint): Any? {
        val startTime = System.nanoTime()
        val name = "${jointPoint.signature.declaringTypeName}.${jointPoint.signature.name}"
        val res = jointPoint.proceed(jointPoint.args)
        method2Executions.computeIfAbsent(name) { mutableListOf() }.add(System.nanoTime() - startTime)
        return res
    }

    fun printExecutionTime() {
        val asciiTable = AsciiTable().apply {
            addRule()
            addRow(
                "method name",
                "total execution time",
                "average execution time",
                "total method calls"
            )
            addRule()
        }

        method2Executions
            .forEach { (k, v) ->
                asciiTable.apply {
                    addRow(k, v.sum(), v.average(), v.size)
                    addRule()
                }
            }

        println(asciiTable.render(150))
    }
}