package clock

import java.time.Instant

class NowClock : Clock {
    override fun now(): Instant = Instant.now()
}