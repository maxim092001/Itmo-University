package clock

import java.time.Instant

interface Clock {
    fun now(): Instant
}