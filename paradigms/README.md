# Тесты к курсу «Парадигмы программирования»

[Условия домашних заданий](http://www.kgeorgiy.info/courses/paradigms/homeworks.html)

Домашнее задание 1. Обработка ошибок
----

#### Реализация: [ExpressionWithExceptions](https://github.com/maxim092001/Itmo-University/tree/master/paradigms/parsing_expression)
Модификации
 * *Базовая*
    * Класс `ExpressionParser` должен реализовывать интерфейс
        [Parser](https://github.com/maxim092001/Itmo-University/blob/master/paradigms/parsing_expression/expression/parser/Parser.java)
    * Классы `CheckedAdd`, `CheckedSubtract`, `CheckedMultiply`,
        `CheckedDivide` и `CheckedNegate` должны реализовывать интерфейс
        [TripleExpression](https://github.com/maxim092001/Itmo-University/blob/master/paradigms/parsing_expression/expression/TripleExpression.java)
    * Нельзя использовать типы `long` и `double`
    * Нельзя использовать методы классов `Math` и `StrictMath`
    * [Исходный код тестов](https://github.com/maxim092001/Itmo-University/blob/master/paradigms/parsing_expression/test/ExceptionsTest.java)
 * *PowLog*
    * Дополнительно реализуйте бинарные операции (максимальный приоритет):
        * `**` – возведение в степень, `2 ** 3` равно 8;
        * `//` – логарифм, `10 // 2` равно 3.
    * [Исходный код тестов](https://github.com/maxim092001/Itmo-University/blob/master/paradigms/parsing_expression/test/ExceptionsPowLogTest.java)

Домашнее задание 2. Бинарный поиск
----

#### Реализация: [BinarySearchWithContracts](https://github.com/maxim092001/Itmo-University/tree/master/paradigms/binary-search-contract)

Модификации
 * *Базовая*
    * Класс `BinarySearch` должен находиться в пакете `search`
    * [Исходный код тестов](https://github.com/maxim092001/Itmo-University/blob/master/paradigms/binary-search-contract/search/BinarySearchTest.java)
 * *Span*
    * Требуется вывести два числа: начало и длину диапазона элементов,
      равных `x`. Если таких элементов нет, то следует вывести
      пустой диапазон, у которого левая граница совпадает с местом
      вставки элемента `x`.
    * Не допускается использование типов `long` и `BigInteger`.
    * Класс должен иметь имя `BinarySearchSpan`
    * [Исходный код тестов](https://github.com/maxim092001/Itmo-University/blob/master/paradigms/binary-search-contract/search/BinarySearchSpanTest.java)

Домашнее задание 3. Очередь на  массиве
----

#### Реализация:[ArrayQueueWithContracts](https://github.com/maxim092001/Itmo-University/tree/master/paradigms/queue-contract)
Модификации
 * *Базовая*
    * Классы должны находиться в пакете `queue`
    * [Исходный код тестов](https://github.com/maxim092001/Itmo-University/tree/master/paradigms/queue-contract/queue/ArrayQueueTest.java)
 * *Deque* (сложная)
    * Реализовать методы
        * `push` – добавить элемент в начало очереди
        * `peek` – вернуть последний элемент в очереди
        * `remove` – вернуть и удалить последний элемент из очереди
    * [Исходный код тестов](https://github.com/maxim092001/Itmo-University/tree/master/paradigms/queue-contract/queue/ArrayQueueDequeTest.java)

Домашнее задание 4. Очередь на связном списке
----

#### Реализация: [LinkedQueueWithContracts](https://github.com/maxim092001/Itmo-University/tree/master/paradigms/queue2.0-contract)

Модификации
 * *Базовая*
    * [Откомпилированные тесты](https://github.com/maxim092001/Itmo-University/tree/master/paradigms/queue2.0-contract/QueueTest.jar)
 * *Functions*
    * Добавить в интерфейс очереди и реализовать методы
        * `filter(predicate)` – создать очередь, содержащую элементы, удовлетворяющие
            [предикату](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/function/Predicate.html)
        * `map(function)` – создать очередь, содержащую результаты применения
            [функции](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/function/Function.html)
    * Исходная очередь должна остаться неизменной
    * Тип возвращаемой очереди должен соответствовать типу исходной очереди
    * Взаимный порядок элементов должен сохраняться
    * Дублирования кода быть не должно
    * [Откомпилированные тесты](https://github.com/maxim092001/Itmo-University/tree/master/paradigms/queue2.0-contract/QueueFunctionsTest.jar)

Домашнее задание 5. Вычисление в различных типах
----

#### Реализация: [GenericExpressions](https://github.com/maxim092001/Itmo-University/tree/master/paradigms/parser-generic)

Модификации
 * *Базовая*
    * Класс `GenericTabulator` должен реализовывать интерфейс
      [Tabulator](https://github.com/maxim092001/Itmo-University/tree/master/paradigms/parser-generic/expression/generic/Tabulator.java) и
      сроить трехмерную таблицу значений заданного выражения.
        * `mode` – режим вычислений:
           * `i` – вычисления в `int` с проверкой на переполнение;
           * `d` – вычисления в `double` без проверки на переполнение;
           * `bi` – вычисления в `BigInteger`.
        * `expression` – выражение, для которого надо построить таблицу;
        * `x1`, `x2` – минимальное и максимальное значения переменной `x` (включительно)
        * `y1`, `y2`, `z1`, `z2` – аналогично для `y` и `z`.
        * Результат: элемент `result[i][j][k]` должен содержать
          значение выражения для `x = x1 + i`, `y = y1 + j`, `z = z1 + k`.
          Если значение не определено (например, по причине переполнения),
          то соответствующий элемент должен быть равен `null`.
    * [Исходный код тестов](https://github.com/maxim092001/Itmo-University/tree/master/paradigms/parser-generic/expression/generic/GenericTest.java)
 * *Сmm*
    * Дополнительно реализовать унарные операции:
        * `count` – число установленных битов, `count 5` равно 2.
    * Дополнительно реализовать бинарную операцию (минимальный приоритет):
        * `min` – минимум, `2 min 3` равно 2;
        * `max` – максимум, `2 max 3` равно 3.
    * [Исходный код тестов](https://github.com/maxim092001/Itmo-University/blob/master/paradigms/parser-generic/expression/generic/GenericCmmTest.java)
 * *Ls*
    * Дополнительно реализовать поддержку режимов:
        * `l` – вычисления в `long` без проверки на переполнение;
        * `s` – вычисления в `short` без проверки на переполнение.
    * [Исходный код тестов](https://github.com/maxim092001/Itmo-University/tree/master/paradigms/parser-generic/expression/generic/GenericLsTest.java)
 * *CmmUls*
    * Реализовать операции из модификации *Cmm*.
    * Дополнительно реализовать поддержку режимов:
        * `u` – вычисления в `int` без проверки на переполнение;
        * `l` – вычисления в `long` без проверки на переполнение;
        * `s` – вычисления в `s` без проверки на переполнение.
    * [Исходный код тестов](https://github.com/maxim092001/Itmo-University/blob/master/paradigms/parser-generic/expression/generic/GenericCmmUlsTest.java)


Домашнее задание 6. Функциональные выражения на JavaScript
----
#### Реализация: [FunctionalExpression](https://github.com/maxim092001/Itmo-University/tree/master/paradigms/functional-expression/functionalExpression.js)
Модификации
 * *Базовая*
    * Код должен находиться в файле `functionalExpression.js`.
 * *PieSinCos*. Дополнительно реализовать поддержку:
    * переменных: `y`, `z`;
    * констант:
        * `pi` – π;
        * `e` – основание натурального логарифма;
    * операций:
        * `sin` – синус, `pi sin` равно 0;
        * `cos` – косинус, `pi cos` равно -1.
 * *Cube*. Дополнительно реализовать поддержку:
    * переменных: `y`, `z`;
    * унарных функций:
        * `cube` – возведение в куб, `2 cube` равно 8;
        * `cuberoot` – кубический корень, `-8 cuberoot` равно -2;
 * *PieAvgMed*. Дополнительно реализовать поддержку:
    * переменных: `y`, `z`;
    * констант:
        * `pi` – π;
        * `e` – основание натурального логарифма;
    * операций:
        * `avg5` – арифметическое среднее пяти аргументов, `1 2 3 4 5 avg5` равно 7.5;
        * `med3` – медиана трех аргументов, `1 2 -10 med3` равно 1.

Домашнее задание 7. Объектные выражения на JavaScript
----
#### Реализация: [ObjectExpression](https://github.com/maxim092001/Itmo-University/tree/master/paradigms/object-expression/objectExpression.js)
Модификации
 * *Базовая*
    * Код должен находиться в файле `objectExpression.js`.
 * *MinMax*. Дополнительно реализовать поддержку:
    * функций:
        * `Min3` (`min3`) – минимум из трех аргументов, `1 2 3 min` равно 1;
        * `Max5` (`max5`) – максимум из пяти аргументов, `1 2 3 4 5 max` равно 5;
 * *PowLog*. Дополнительно реализовать поддержку:
    * бинарных операций:
        * `Power` (`pow`) – возведение в степень, `2 3 pow` равно 8;
        * `Log` (`log`) – логарифм абсолютного значения аргумента
            по абсолютному значению основания `-2 -8 log` равно 3;
