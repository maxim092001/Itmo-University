package com.sd.aop.cache

interface LruCache<K, V> {
    operator fun get(key: K): V?
    operator fun set(key: K, value: V)
    fun isEmpty(): Boolean
    fun size(): Int
    fun contains(key: K): Boolean
}

class LruCacheImpl<K, V>(private val capacity: Int) : LruCache<K, V> {

    private var head: Node = Node(null, null)
    private var tail: Node = Node(null, null)
    private val map: MutableMap<K, Node> = HashMap()

    init {
        require(capacity >= 1)

        head.next = tail
        tail.prev = head
    }

    override operator fun get(key: K): V? {
        require(map.size <= capacity)
        val prevSize = map.size

        val node = map[key] ?: return null
        move(node)

        val curSize = map.size
        require(prevSize == curSize)
        require(head.next!!.k == key)
        return node.v
    }

    override operator fun set(key: K, value: V) {
        require(map.size <= capacity)
        val prevSize = map.size

        val node = map[key]

        if (node == null) {
            val newNode = Node(key, value)
            map[key] = newNode
            addNode(newNode)

            if (map.size > capacity) {
                val lastNode = removeLastNode()
                map.remove(lastNode.k)
            }
        } else {
            node.v = value
            move(node)
        }

        require(map.size <= capacity)
        val newSize = map.size
        require(newSize >= prevSize)
        require(head.next!!.k == key)
        require(head.next!!.v == value)
    }

    private fun addNode(node: Node) {
        node.prev = head
        node.next = head.next

        head.next!!.prev = node
        head.next = node
    }

    private fun removeNode(node: Node) {
        val prev = node.prev
        val next = node.next

        prev!!.next = next
        next!!.prev = prev
    }

    private fun move(node: Node) {
        removeNode(node)
        addNode(node)
    }

    private fun removeLastNode(): Node {
        val res = tail.prev
        removeNode(res!!)
        return res
    }

    inner class Node(val k: K?, var v: V?) {
        var next: Node? = null
        var prev: Node? = null
    }

    override fun isEmpty(): Boolean {
        return map.isEmpty()
    }

    override fun size(): Int {
        return map.size
    }

    override fun contains(key: K): Boolean {
        return map.containsKey(key)
    }
}