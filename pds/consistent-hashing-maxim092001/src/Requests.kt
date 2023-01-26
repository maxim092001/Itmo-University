import kotlinx.serialization.Serializable

@Serializable
sealed class Request

@Serializable
data class AddShardRequest(
    val newShard: Shard, val vnodeHashes: Set<Int>,
    val result: List<Pair<Shard, Set<HashRange>>>
) : Request()

@Serializable
data class RemoveShardRequest(
    val shard: Shard,
    val result: List<Pair<Shard, Set<HashRange>>>
) : Request()

@Serializable
data class GetShardByKeyRequest(val key: String, val result: Shard) : Request()
