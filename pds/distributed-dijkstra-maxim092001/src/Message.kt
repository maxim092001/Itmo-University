package dijkstra.messages

import kotlin.jvm.JvmInline

sealed class Message

object AcknowledgementMessage : Message()

object RemoveChildMessage : Message()

object NewChildMessage : Message()

data class DistanceMessage(val distance: Values.Distance) : Message()

object Values {
    @JvmInline
    value class Distance(val value: Long)
}