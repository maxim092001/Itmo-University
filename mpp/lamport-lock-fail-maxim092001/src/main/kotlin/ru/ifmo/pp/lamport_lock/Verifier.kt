package ru.ifmo.pp.lamport_lock

import java.io.BufferedReader
import java.io.FileReader
import java.util.Collections.synchronizedList
import java.util.stream.*

fun main(args: Array<String>) {
    val t1 = LockThread(0)
    val t2 = LockThread(1)
    t1.start()
    t2.start()
    t1.join()
    t2.join()
    if (acquired != 2) {
        println("Wrong execution: only $acquired threads entered in critical section")
        System.exit(1)
    }
    println("Correct example")
}

var error = false
var acquired = 0

class LockThread(val id: Int) : Thread() {
    override fun run() {
        lock(id)
        acquired++
    }
}

val label = arrayOf(0, 0)
val N = label.size

fun lock(id: Int) {
    var my = 1;
    for (k in 0..N - 1) if (k != id) {
        waitForRead(id, "label[$k]")
        val l = label[k];
        checkRead(l.toString())
        my = maxOf(my, l + 1)
    }
    waitForWrite(id, "label[$id]", my.toString())
    label[id] = my
    for (k in 0..N - 1) if (k != id) {
        while (true) {
            waitForRead(id, "label[$k]")
            val other = label[k];
            checkRead(other.toString())
            if (other == 0 || other > my || (other == my && k > id))
                break
        }
    }
}

private val execution: MutableList<Action> = synchronizedList(parseExecution("execution"));

fun waitForRead(id: Int, loc: String) {
    waitForCurThreadAction(id)
    checkLocation(id, loc)
    println("Start read operation by thread #$id from $loc")
}

fun checkRead(res: String) {
    checkValue(res)
    println("Read operation completed, result=$res")
    execution.removeAt(0)
}

fun waitForWrite(id: Int, loc: String, value: String) {
    waitForCurThreadAction(id)
    checkLocation(id, loc)
    checkValue(value)
    println("Write operation by thread #$id to $loc, value=$value")
    execution.removeAt(0)
}

private fun waitForCurThreadAction(id: Int) {
    try {
        while (execution[0].tid != id);
    } catch (e: IndexOutOfBoundsException) {
        println("Performing action in thread #$id, but no more actions in execution")
        error = true;
        System.exit(1);
    }
}

private fun checkLocation(id: Int, loc: String) {
    if (execution[0].loc != loc) {
        println("$id, Invalid location, cur=$loc, expected_action=${execution[0]}")
        System.exit(1);
    }
}

private fun checkValue(value: String) {
    if (execution[0].value != value) {
        println("Invalid value, cur=$value, expected_action=${execution[0]}")
        System.exit(1);
    }
}

data class Action(
        val tid: Int,
        val type: ExType,
        val loc: String,
        val value: String
)

enum class ExType { READ, WRITE }

private fun parseAction(s: String): Action {
    val parts = s.trim().split(" ");
    return Action(tid = parts[0].toInt(),
            type = if (parts[2] == "rd") ExType.READ else ExType.WRITE,
            loc = parts[3],
            value = parts[4])
}

private fun parseExecution(filename: String): List<Action> {
    BufferedReader(FileReader(filename)).use { br ->
        return br.lines().map(::parseAction).collect(Collectors.toList())
    }
}
