import clock.SettableClock
import org.junit.Before
import org.junit.Test
import stats.EventStatistics
import stats.RpmEventStatistics
import kotlin.math.abs
import java.time.Duration
import java.time.Instant
import kotlin.test.assertTrue

class RpmEventStatisticsTest {
    private var clock: SettableClock = SettableClock(Instant.now())
    private var stats: EventStatistics = RpmEventStatistics(clock)

    @Before
    fun cleanData() {
        val now = Instant.now()
        clock = SettableClock(now)
        stats = RpmEventStatistics(clock)
    }

    @Test
    fun defaultTest() {
        stats.incEvent("1")
        stats.incEvent("1")
        stats.incEvent("2")

        val expected = mapOf(
            "1" to 2.0 / 60.0,
            "2" to 1.0 / 60.0
        )
        val actual = stats.getAllEventStatistics()

        assertTrue(expected equalWithEps actual)
    }

    @Test
    fun emptyStatsTest() {
        val expected: Map<String, Double> = mapOf()
        val actual = stats.getAllEventStatistics()

        assertTrue(expected equalWithEps actual)
    }

    @Test
    fun outdatedEventsTest() {
        stats.incEvent("1")
        stats.incEvent("2")
        clock.setNow(clock.now() + Duration.ofHours(1) + Duration.ofMinutes(1))

        val expected: Map<String, Double> = mapOf()
        val actual = stats.getAllEventStatistics()

        assertTrue(expected equalWithEps actual)
    }

    @Test
    fun someEventsOutdatedTest() {
        stats.incEvent("1")
        stats.incEvent("2")

        clock.setNow(clock.now() + Duration.ofMinutes(40))

        stats.incEvent("1")
        stats.incEvent("2")
        stats.incEvent("3")

        clock.setNow(clock.now() + Duration.ofMinutes(30))

        val expected = mapOf(
            "1" to 1.0 / 60.0,
            "2" to 1.0 / 60.0,
            "3" to 1.0 / 60.0
        )

        val actual = stats.getAllEventStatistics()
        assertTrue(expected equalWithEps actual)
    }


    companion object {
        private const val EPS = 1e-5

        infix fun Map<String, Double>.equalWithEps(arg: Map<String, Double>): Boolean {
            return keys == arg.keys
                    && arg.all { (k, v) -> abs(get(k)!! - v) <= EPS }
        }
    }
}