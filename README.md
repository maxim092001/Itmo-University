## [Введение в программирование](https://github.com/maxim092001/Itmo-University/tree/master/prog-intro-homework)
[**Условия домашних заданий**](http://www.kgeorgiy.info/courses/prog-intro/homeworks.html)

#### Домашнее задание 2. Сумма чисел

*Реализация*: [sumDouble](https://github.com/maxim092001/Itmo-University/tree/master/prog-intro-homework/sumDouble)

Модификация:
* *Double*
    * Входные данные являются 64-битными числами с формате с плавающей точкой
    * Класс должен иметь имя `SumDouble`
    * [Исходный код тестов](https://github.com/maxim092001/Itmo-University/blob/master/prog-intro-homework/sumDouble/SumDoubleTest.java)
    * [Откомпилированные тесты](https://github.com/maxim092001/Itmo-University/blob/master/prog-intro-homework/sumDouble/SumDoubleTest.jar?raw=true)

#### Домашнее задание 3. Быстрый Scanner + Реверс.

*Реализация*: [myScanner](https://github.com/maxim092001/Itmo-University/tree/master/prog-intro-homework/myScanner)

Свой собственный Scanner.

*Реализация*: [reverseMin](https://github.com/maxim092001/Itmo-University/tree/master/prog-intro-homework/reverseMin)

Модификация:
* *Min*
    * Рассмотрим входные данные как (не полностью определенную) матрицу,
      вместо каждого числа выведите минимум из чисел в его столбце и строке
    * Класс должен иметь имя `ReverseMin`
    * [Исходный код тестов](https://github.com/maxim092001/Itmo-University/blob/master/prog-intro-homework/reverseMin/FastReverseMinTest.java)
    * [Откомпилированные тесты](https://github.com/maxim092001/Itmo-University/blob/master/prog-intro-homework/reverseMin/FastReverseMinTest.jar?raw=true)

#### Домашнее задание 4. Подсчет слов++

*Реализация*: [wordStatLastIndex](https://github.com/maxim092001/Itmo-University/tree/master/prog-intro-homework/wordStatLastIndex)

Модификация:
* *LastIndex*
    * Вместо номеров вхождений во всем файле надо указывать
      только последнее вхождение в каждой строке
    * Класс должен иметь имя `WordStatLastIndex`
    * [Исходный код тестов](https://github.com/maxim092001/Itmo-University/blob/master/prog-intro-homework/wordStatLastIndex/WordStatLastIndexTest.java)
    * [Откомпилированные тесты](https://github.com/maxim092001/Itmo-University/blob/master/prog-intro-homework/wordStatLastIndex/WordStatLastIndexTest.jar?raw=true)
    
#### Домашнее задание 5. Разметка

*Реализация*: [markup](https://github.com/maxim092001/Itmo-University/tree/master/prog-intro-homework/markup)

Модификация:

* *Tex списки*
    * Добавьте поддержку:
      * Нумерованных списков (класс `OrderedList`, окружение `enumerate`): последовательность элементов
      * Ненумерованных списков (класс `UnorderedList`, окружение `itemize`): последовательность элементов
      * Элементов списка (класс `ListItem`, тег `\item`: последовательность абзацев и списков
    * Для новых классов поддержка Markdown не требуется
    * [Исходный код тестов](https://github.com/maxim092001/Itmo-University/blob/master/prog-intro-homework/markup/TexListTest.java)

#### Домашнее задание 6. Markdown to HTML

*Реализация*:  [md2html](https://github.com/maxim092001/Itmo-University/tree/master/prog-intro-homework/md2html)

Модификации (реализованы все):

 * *Underline*
    * Добавьте поддержку `++подчеркивания++`: `<u>подчеркивания</u>`
    * [Исходный код тестов](https://github.com/maxim092001/Itmo-University/blob/master/prog-intro-homework/md2html/tests/Md2HtmlUnderlineTest.java)
    * [Откомпилированные тесты](https://github.com/maxim092001/Itmo-University/blob/master/prog-intro-homework/md2html/tests/Md2HtmlUnderlineTest.jar?raw=true)

 * *Link*
    * Добавьте поддержку ```[ссылок с _выделением_](https://kgeorgiy.info)```:
        ```&lt;a href='https://kgeorgiy.info'>ссылок с &lt;em>выделением&lt;/em>&lt;/a>```
    * [Исходный код тестов](https://github.com/maxim092001/Itmo-University/blob/master/prog-intro-homework/md2html/tests/Md2HtmlLinkTest.java)
    * [Откомпилированные тесты](https://github.com/maxim092001/Itmo-University/blob/master/prog-intro-homework/md2html/tests/Md2HtmlLinkTest.jar?raw=true)
 * *Mark*
    * Добавьте поддержку `~выделения цветом~`: `<mark>выделения цветом</mark>`
    * [Исходный код тестов](https://github.com/maxim092001/Itmo-University/blob/master/prog-intro-homework/md2html/tests/Md2HtmlMarkTest.java)
    * [Откомпилированные тесты](https://github.com/maxim092001/Itmo-University/blob/master/prog-intro-homework/md2html/tests/Md2HtmlMarkTest.jar?raw=true)
 * *Image*
    * Добавьте поддержку ```![картинок](http://www.ifmo.ru/images/menu/small/p10.jpg)```:
        ```&lt;img alt='картинок' src='http://www.ifmo.ru/images/menu/small/p10.jpg'&gt;```
    * [Исходный код тестов](https://github.com/maxim092001/Itmo-University/blob/master/prog-intro-homework/md2html/tests/Md2HtmlImageTest.java)
    * [Откомпилированные тесты](https://github.com/maxim092001/Itmo-University/blob/master/prog-intro-homework/md2html/tests/Md2HtmlImageTest.jar?raw=true)

#### Домашнее задание 10. Игра n,m,k

*Реализация*: [mnkGame](https://github.com/maxim092001/Itmo-University/tree/master/prog-intro-homework/markup)

Модификация:
 * *Матчи*
    * Добавьте поддержку матчей: последовательность игр указанного числа побед
    * Стороны в матче должны меняться каждую игру

#### Домашнее задание 11. Выражения

*Реализация*: [expression](https://github.com/maxim092001/Itmo-University/tree/master/prog-intro-homework/expression)

Модификация:

 * *Triple*
    * Дополнительно реализуйте интерфейс [TripleExpression](https://github.com/maxim092001/Itmo-University/blob/master/prog-intro-homework/expression/TripleExpression.java)
    * [Исходный код тестов](https://github.com/maxim092001/Itmo-University/blob/master/prog-intro-homework/expression/TripleExpressionTest.java)
