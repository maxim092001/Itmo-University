package clock

import java.time.Instant

class SettableClock(private var now: Instant) : Clock {

    fun setNow(i: Instant) {
        now = i
    }

    override fun now(): Instant = now
}