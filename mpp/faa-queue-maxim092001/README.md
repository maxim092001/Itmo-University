# FAA-Based Queue
В этом задании вам необходимо реализовать алгоритм очереди, построенного на списке сегментов, 
где в каждом сегменте хранится массив с элементами очереди. Основная синхронизация внутри сегмента 
выполняется при помощи операции `Fetch-And-Add` (см. `getAndIncrement()` на соответствующих примитивах), 
в то время как сегменты образуют односвязные список по подобию очереди Майкла-Скотта.
Все подробности -- в соответствующей лекции.

В файле [`src/FAAQueue.kt`](src/FAAQueue.kt) уже содержится однопоточная версия, вам нужно лишь 
доработать её до многопоточной. Реализация должна быть lock-free.

Полезные статьи:
* [Fast Concurrent Queues for x86 Processors](https://www.cs.tau.ac.il/~mad/publications/ppopp2013-x86queues.pdf)
* [A Wait-free Queue as Fast as Fetch-and-Add](http://chaoran.me/assets/pdf/wfq-ppopp16.pdf)

Для проверки вашего решения запустите из корня репозитория:
* `./gradlew build` на Linux или MacOS
* `gradlew build` на Windows