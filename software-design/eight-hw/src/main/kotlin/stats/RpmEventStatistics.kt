package stats

import clock.Clock
import java.time.Duration
import java.time.Instant
import java.util.*
import kotlin.collections.HashMap

class RpmEventStatistics(private val clock: Clock) : EventStatistics {

    private val eventToCount: MutableMap<String, Int> = HashMap()
    private val eventsBuffer: Deque<Pair<String, Instant>> = ArrayDeque()

    override fun incEvent(name: String) {
        val now = clock.now()
        removeOutdated(now)
        eventsBuffer.add(name to now)
        eventToCount.compute(name) { _, count -> count?.plus(1) ?: 1 }
    }

    override fun getEventStatisticsByName(name: String): Double {
        val now = clock.now()
        removeOutdated(now)
        return eventToCount[name]?.toDouble()?.div(60) ?: 0.0
    }

    override fun getAllEventStatistics(): Map<String, Double> {
        val now = clock.now()
        removeOutdated(now)
        return eventToCount.mapValues { it.value.toDouble() / 60.0 }
    }

    override fun printStatistics() {
        getAllEventStatistics().forEach { (name, count) ->
            println("$name: $count")
        }
    }

    private fun removeOutdated(t: Instant) {
        val outdatedEvents = eventsBuffer.takeWhile {
            Duration.between(it.second, t).toHours() >= 1L
        }

        eventsBuffer.drop(outdatedEvents.size)

        outdatedEvents.groupBy { it.first }
            .mapValues { it.value.size }
            .forEach { (event, count) ->
                if (eventToCount[event] == count)
                    eventToCount.remove(event)
                else
                    eventToCount.compute(event) { _, v -> v!! - count }
            }
    }
}