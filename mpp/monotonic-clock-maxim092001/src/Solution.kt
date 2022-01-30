/**
 * В теле класса решения разрешено использовать только переменные делегированные в класс RegularInt.
 * Нельзя volatile, нельзя другие типы, нельзя блокировки, нельзя лазить в глобальные переменные.
 *
 * @author : Grankin Maksim
 */
class Solution : MonotonicClock {
    private var c1 by RegularInt(0)
    private var c2 by RegularInt(0)
    private var c3 by RegularInt(0)
    private var q1 by RegularInt(0)
    private var q2 by RegularInt(0)

    override fun write(time: Time) {
        // write right-to-left
        c1 = time.d1
        c2 = time.d2
        c3 = time.d3
        q2 = time.d2
        q1 = time.d1
    }

    override fun read(): Time {

        // read left-to-right
        val q1Fixed = q1
        val q2Fixed = q2
        val c3Fixed = c3
        val c2Fixed = c2
        val c1Fixed = c1
        return Time(
            c1Fixed,
            if (q1Fixed == c1Fixed) c2Fixed else 0,
            if (q1Fixed == c1Fixed && q2Fixed == c2Fixed) c3Fixed else 0
        )
    }

}