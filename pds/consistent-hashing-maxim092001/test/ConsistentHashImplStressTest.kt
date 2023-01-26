import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions.*
import java.nio.file.Files
import java.nio.file.Paths
import java.util.stream.Collectors
import kotlinx.serialization.*
import kotlinx.serialization.json.*

class ConsistentHashImplStressTest {
    @Test
    fun stress() {
        val path = Paths.get("tests")
        Files.list(path).forEach { curPath ->
            if(curPath.toString().endsWith(".json")) {
                val curJson = Files.newBufferedReader(curPath).lines().collect(Collectors.joining())
                val curTests = Json.decodeFromString<List<Request>>(curJson)
                val cHash = ConsistentHashImpl<String>()
                for (curTest in curTests) {
                    when (curTest) {
                        is AddShardRequest -> {
                            val result = cHash.addShard(curTest.newShard, curTest.vnodeHashes)
                            assertEquals(result, curTest.result.toMap())
                        }
                        is RemoveShardRequest -> {
                            val result = cHash.removeShard(curTest.shard)
                            assertEquals(result, curTest.result.toMap())
                        }
                        is GetShardByKeyRequest -> {
                            val result = cHash.getShardByKey(curTest.key)
                            assertEquals(result, curTest.result)
                        }
                    }
                }
            }
        }
    }
}