package raft.system

import java.io.*
import java.util.*

const val HEARTBEAT_TIMEOUT_MS_PROP = "heartbeatTimeoutMs"
const val HEARTBEAT_RANDOM_PROP = "heartbeatRandom"

object Configuration {
    // Use defaults from file, override from command-line system properties
    private val systemProps = Properties(loadPropsFile()).apply { putAll(System.getProperties()) }

    private fun loadPropsFile() =
        Properties().also { props ->
            File("system.properties").inputStream().use { input ->
                props.load(input)
            }
        }

    val nodes: List<Node> = ArrayList<Node>().apply {
        var i = 0
        while (true) {
            val key = "node.${i + 1}"
            val addr = systemProps.getProperty(key) ?: break
            i++
            add(Node(i, addr.substringBefore(":"), addr.substringAfter(":").toInt()))
        }
    }

    val heartbeatTimeoutMs = systemProps.getProperty(HEARTBEAT_TIMEOUT_MS_PROP).toInt()
    val heartbeatRandom = systemProps.getProperty(HEARTBEAT_RANDOM_PROP).toBoolean()
}

