/**
 * Время из трех целых чисел.
 */
data class Time(val d1: Int, val d2: Int, val d3: Int) : Comparable<Time> {
    override fun compareTo(other: Time): Int =
        compareValuesBy(this, other, Time::d1, Time::d2, Time::d3)
}

/**
 * Монотонные часы.
 */
interface MonotonicClock {
    fun write(time: Time)
    fun read(): Time
}
