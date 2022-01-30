import java.util.*
import kotlin.math.sqrt

class Stats {
    private var n = 0
    private var mean = 0.0
    private var nvar = 0.0

    private val dev: Double
        get() = sqrt(nvar / (n - 1))

    fun add(x: Long) {
        n++
        val prev = mean
        mean += (x - prev) / n
        nvar += (x - prev) * (x - mean)
    }

    override fun toString(): String = String.format(Locale.US, "%,.0f +- %,.0f", mean, dev)
}