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
