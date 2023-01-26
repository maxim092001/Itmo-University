import kotlinx.serialization.*

@Serializable
data class HashRange(val leftBorder: Int, val rightBorder: Int)
