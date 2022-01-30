# Synchronous Queue
В этом задании вам необходимо реализовать алгоритм для synchronous queue, который основан на очереди Michael и Scott.
Исходную работу вы можете найти по этой [ссылке](https://www.cs.rochester.edu/u/scott/papers/2009_Scherer_CACM_SSQ.pdf).

В отличие от реализации в Java (см. класс `java.util.concurrent.SynchronousQueue`), 
мы будем реализовывать этот алгоритм для корутин, использую соответствующий механизм 
для засыпания и пробуждения. Смотрите класс `SynchronousQueueSequential` 
в [`SynchronousQueueTest.kt`](test/SynchronousQueueTest.kt) в качестве примера.

В файле [`src/SynchronousQueue.kt`](src/SynchronousQueue.kt) находится описание интерфейса synchronous queue, 
который  вам предстоит реализовать. Ваше решение должно быть в файле [`src/SynchronousQueueMS.kt`](src/SynchronousQueueMS.kt).

При реализации алгоритма вам может потребоваться начать всю операцию заново после вызова `suspendCoroutine`, 
вы можете выразить эту логику следующим образом: 
```kotlin 
val res = suspendCoroutine<Any> sc@ { cont ->
  ...
  if (shouldRetry()) {
    cont.resume(RETRY)
    return@sc
  }
  ...
}
if (res === RETRY) continue
```                

Для проверки вашего решения запустите из корня репозитория:
* `./gradlew build` на Linux или MacOS
* `gradlew build` на Windows