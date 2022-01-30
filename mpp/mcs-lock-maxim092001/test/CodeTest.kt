import org.junit.*
import java.lang.reflect.*
import java.util.concurrent.atomic.*

class CodeTest {
    @Test
    fun testSolutionClass() {
        check(Lock::class.java.isAssignableFrom(Solution::class.java))
        validateClass(Solution::class.java, HashSet())
    }

    private fun validateClass(clazz: Class<*>, visited: HashSet<Class<*>>) {
        if (!visited.add(clazz)) return
        check(clazz.name == "Solution" || clazz.name.startsWith("Solution$")) {
            "$clazz: Must be 'Solution' or its inner class"
        }
        for (f in clazz.declaredFields) {
            check(!Modifier.isStatic(f.modifiers)) { "$clazz: Cannot use static fields" }
            check(Modifier.isFinal(f.modifiers)) { "$clazz: All fields must be final" }
            when (f.type) {
                AtomicReference::class.java -> {
                    val t = f.genericType as ParameterizedType
                    val a = t.actualTypeArguments[0]
                    when (a) {
                        java.lang.Boolean::class.java -> { /* ok */ }
                        is Class<*> -> {
                            try {
                                validateClass(a, visited)
                            } catch (e: Throwable) {
                                throw IllegalStateException("Bad class in type parameter of field $f in class $clazz", e)
                            }
                        }
                        else -> error("Unexpected type parameter of field $f in class $clazz")
                    }
                }
                Environment::class.java, Thread::class.java -> { /* ok */ }
                else -> try {
                    validateClass(f.type, visited)
                } catch (e: Throwable) {
                    throw IllegalStateException("Bad class in field $f in class $clazz", e)
                }
            }
        }
        if (clazz.superclass != Any::class.java) validateClass(clazz.superclass, visited)
    }
}