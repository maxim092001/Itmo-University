/**
 * Bank implementation.
 * This implementation is not thread-safe.
 */
class SequentialBank(n: Int) : Bank {
    /**
     * An array of accounts by index.
     */
    private val accounts = Array(n) { Account() }

    override val numberOfAccounts: Int
        get() = accounts.size

    override fun getAmount(index: Int): Long = accounts[index].amount

    override val totalAmount: Long
        get() = accounts.fold(0L) { s, a -> s + a.amount }

    override fun deposit(index: Int, amount: Long): Long {
        require(amount > 0) { "Invalid amount: $amount" }
        val account = accounts[index]
        check(amount <= MAX_AMOUNT && account.amount + amount <= MAX_AMOUNT) { "Overflow" }
        account.amount += amount
        return account.amount
    }

    override fun withdraw(index: Int, amount: Long): Long {
        require(amount > 0) { "Invalid amount: $amount" }
        val account = accounts[index]
        check(account.amount - amount >= 0) { "Underflow" }
        account.amount -= amount
        return account.amount
    }

    override fun transfer(fromIndex: Int, toIndex: Int, amount: Long) {
        require(amount > 0) { "Invalid amount: $amount" }
        require(fromIndex != toIndex) { "fromIndex == toIndex" }
        val from = accounts[fromIndex]
        val to = accounts[toIndex]
        check(amount <= from.amount) { "Underflow" }
        check(!(amount > MAX_AMOUNT || to.amount + amount > MAX_AMOUNT)) { "Overflow" }
        from.amount -= amount
        to.amount += amount
    }

    /**
     * Private account data structure.
     */
    private class Account(var amount: Long = 0)
}