/**
 * Operation on a bank.
 */
sealed class Operation {
    operator fun invoke(bank: Bank): Any = try {
        invokeImpl(bank)
    } catch (t: Throwable) {
        t.javaClass
    }

    abstract fun invokeImpl(bank: Bank): Any

    class GetAmount(val index: Int) : Operation() {
        override fun invokeImpl(bank: Bank) = bank.getAmount(index)
        override fun toString(): String = "GetAmount{index=$index}"
    }

    class GetTotalAmount : Operation() {
        override fun invokeImpl(bank: Bank) = bank.totalAmount
        override fun toString(): String = "GetTotalAmount{}"
    }

    class Deposit(val index: Int, val amount: Long) : Operation() {
        override fun invokeImpl(bank: Bank) = bank.deposit(index, amount)
        override fun toString(): String = "Deposit{index=$index, amount=$amount}"
    }

    class Withdraw(val index: Int, val amount: Long) : Operation() {
        override fun invokeImpl(bank: Bank) = bank.withdraw(index, amount)
        override fun toString(): String = "Withdraw{index=$index, amount=$amount}"
    }

    class Transfer(val fromIndex: Int, val toIndex: Int, val amount: Long) : Operation() {
        override fun invokeImpl(bank: Bank) = bank.transfer(fromIndex, toIndex, amount)
        override fun toString(): String = "Transfer{fromIndex=$fromIndex, toIndex=$toIndex, amount=$amount}"
    }
}