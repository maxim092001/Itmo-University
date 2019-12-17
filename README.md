## [Введение в программирование](https://github.com/maxim092001/Itmo-University/tree/master/prog-intro-homework)
[**Условия домашних заданий**](http://www.kgeorgiy.info/courses/prog-intro/homeworks.html)

#### Домашнее задание 2. Сумма чисел

Реализация: *[sumDouble](https://github.com/maxim092001/Itmo-University/tree/master/prog-intro-homework/sumDouble)

* *Double*
    * Входные данные являются 64-битными числами с формате с плавающей точкой
    * Класс должен иметь имя `SumDouble`
    * [Исходный код тестов](java/sum/SumDoubleTest.java)
    * [Откомпилированные тесты](artifacts/sum/SumDoubleTest.jar)

#### Домашнее задание 3. Быстрый Scanner + Реверс.

* [myScanner](https://github.com/maxim092001/Itmo-University/tree/master/prog-intro-homework/myScanner)
Свой более быстрый сканнер

* [reverseMin](https://github.com/maxim092001/Itmo-University/tree/master/prog-intro-homework/reverseMin)
* *Min*
    * Рассмотрим входные данные как (не полностью определенную) матрицу,
      вместо каждого числа выведите минимум из чисел в его столбце и строке
    * Класс должен иметь имя `ReverseMin`
    * [Исходный код тестов](java/reverse/FastReverseMinTest.java)
    * [Откомпилированные тесты](artifacts/reverse/FastReverseMinTest.jar)

#### Домашнее задание 4. Подсчет слов++

* [wordStatLastIndex](https://github.com/maxim092001/Itmo-University/tree/master/prog-intro-homework/wordStatLastIndex)
* *LastIndex*
    * Вместо номеров вхождений во всем файле надо указывать
      только последнее вхождение в каждой строке
    * Класс должен иметь имя `WordStatLastIndex`
    * [Исходный код тестов](java/wordStat/WordStatLastIndexTest.java)
    * [Откомпилированные тесты](artifacts/wordStat/WordStatLastIndexTest.jar)
    
#### Домашнее задание 5. Разметка

* [markup](https://github.com/maxim092001/Itmo-University/tree/master/prog-intro-homework/markup)
 * *Tex списки*
    * Добавьте поддержку:
      * Нумерованных списков (класс `OrderedList`, окружение `enumerate`): последовательность элементов
      * Ненумерованных списков (класс `UnorderedList`, окружение `itemize`): последовательность элементов
      * Элементов списка (класс `ListItem`, тег `\item`: последовательность абзацев и списков
    * Для новых классов поддержка Markdown не требуется
    * [Исходный код тестов](java/markup/TexListTest.java)

#### Домашнее задание 6. Markdown to HTML

* [md2html](https://github.com/maxim092001/Itmo-University/tree/master/prog-intro-homework/md2html)
Модификации (реализованы все)
 * *Underline*
    * Добавьте поддержку `++подчеркивания++`: `<u>подчеркивания</u>`
    * [Исходный код тестов](java/md2html/Md2HtmlUnderlineTest.java)
    * [Откомпилированные тесты](artifacts/md2html/Md2HtmlUnderlineTest.jar)

 * *Link*
    * Добавьте поддержку ```[ссылок с _выделением_](https://kgeorgiy.info)```:
        ```&lt;a href='https://kgeorgiy.info'>ссылок с &lt;em>выделением&lt;/em>&lt;/a>```
    * [Исходный код тестов](java/md2html/Md2HtmlLinkTest.java)
    * [Откомпилированные тесты](artifacts/md2html/Md2HtmlLinkTest.jar)
 * *Mark*
    * Добавьте поддержку `~выделения цветом~`: `<mark>выделения цветом</mark>`
    * [Исходный код тестов](java/md2html/Md2HtmlMarkTest.java)
    * [Откомпилированные тесты](artifacts/md2html/Md2HtmlMarkTest.jar)
 * *Image*
    * Добавьте поддержку ```![картинок](http://www.ifmo.ru/images/menu/small/p10.jpg)```:
        ```&lt;img alt='картинок' src='http://www.ifmo.ru/images/menu/small/p10.jpg'&gt;```
    * [Исходный код тестов](java/md2html/Md2HtmlImageTest.java)
    * [Откомпилированные тесты](artifacts/md2html/Md2HtmlImageTest.jar)

#### Домашнее задание 10. Игра n,m,k

* [mnkGame](https://github.com/maxim092001/Itmo-University/tree/master/prog-intro-homework/markup)
Модификация
 * *Матчи*
    * Добавьте поддержку матчей: последовательность игр указанного числа побед
    * Стороны в матче должны меняться каждую игру

#### Домашнее задание 11. Выражения

* [expression](https://github.com/maxim092001/Itmo-University/tree/master/prog-intro-homework/expression)
Модификация
 * *Triple*
    * Дополнительно реализуйте интерфейс [TripleExpression](java/expression/TripleExpression.java)
    * [Исходный код тестов](java/expression/TripleExpressionTest.java)
