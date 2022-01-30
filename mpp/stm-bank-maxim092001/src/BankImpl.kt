/**
 * Bank implementation. This class is implemented using Software Transaction Manager (STM).
 */
class BankImpl(override val numberOfAccounts: Int) : Bank {
    private val accounts = Array(numberOfAccounts) { TxVar(0L) }

    override fun getAmount(index: Int): Long = atomic {
        accounts[index].read()
    }

    override val totalAmount: Long get() = atomic {
        accounts.fold(0L) { s, a -> s + a.read() }
    }

    override fun deposit(index: Int, amount: Long): Long = atomic {
        require(amount > 0) { "Invalid amount: $amount" }
        val a = accounts[index]
        val update = a.read() + amount
        check(update <= MAX_AMOUNT) { "Overflow" }
        a.write(update)
    }

    override fun withdraw(index: Int, amount: Long): Long = atomic {
        require(amount > 0) { "Invalid amount: $amount" }
        val a = accounts[index]
        val update = a.read() - amount
        check(update >= 0) { "Underflow" }
        a.write(update)
    }

    override fun transfer(fromIndex: Int, toIndex: Int, amount: Long) = atomic<Unit> {
        require(amount > 0) { "Invalid amount: $amount" }
        require(fromIndex != toIndex) { "fromIndex == toIndex" }
        check(amount <= MAX_AMOUNT) { "Underflow/overflow" }
        val from = accounts[fromIndex]
        val to = accounts[toIndex]
        val fromUpdate = from.read() - amount
        val toUpdate = to.read() + amount
        check(toUpdate <= MAX_AMOUNT) { "Overflow" }
        check(fromUpdate >= 0) { "Underflow" }
        from.write(fromUpdate)
        to.write(toUpdate)
    }
}