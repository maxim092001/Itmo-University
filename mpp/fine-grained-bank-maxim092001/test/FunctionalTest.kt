import kotlin.test.*

private val N = 10

/**
 * Functional single-threaded test-suite for bank implementation.
 */
class FunctionalTest {
    private val bank: Bank = BankImpl(N)

    @Test
    fun testEmptyBank() {
        assertEquals(N, bank.numberOfAccounts)
        assertEquals(0, bank.totalAmount)
        for (i in 0 until N) assertEquals(0, bank.getAmount(i))
    }

    @Test
    fun testDeposit() {
        val amount: Long = 1234
        val result = bank.deposit(1, amount)
        assertEquals(amount, result)
        assertEquals(amount, bank.getAmount(1))
        assertEquals(amount, bank.totalAmount)
    }

    @Test
    fun testWithdraw() {
        val depositAmount = 2345
        val depositResult = bank.deposit(1, depositAmount.toLong())
        assertEquals(depositAmount.toLong(), depositResult)
        assertEquals(depositAmount.toLong(), bank.getAmount(1))
        assertEquals(depositAmount.toLong(), bank.totalAmount)
        val withdrawAmount: Long = 1234
        val withdrawResult = bank.withdraw(1, withdrawAmount)
        assertEquals(depositAmount - withdrawAmount, withdrawResult)
        assertEquals(depositAmount - withdrawAmount, bank.getAmount(1))
        assertEquals(depositAmount - withdrawAmount, bank.totalAmount)
    }

    @Test
    fun testTotalAmount() {
        val deposit1: Long = 4567
        val depositResult1 = bank.deposit(1, deposit1)
        assertEquals(deposit1, depositResult1)
        assertEquals(deposit1, bank.totalAmount)
        val deposit2: Long = 6789
        val depositResult2 = bank.deposit(2, deposit2)
        assertEquals(deposit2, depositResult2)
        assertEquals(deposit2, bank.getAmount(2))
        assertEquals(deposit1 + deposit2, bank.totalAmount)
    }

    @Test
    fun testTransfer() {
        val depositAmount = 9876
        val depositResult = bank.deposit(1, depositAmount.toLong())
        assertEquals(depositAmount.toLong(), depositResult)
        assertEquals(depositAmount.toLong(), bank.getAmount(1))
        assertEquals(depositAmount.toLong(), bank.totalAmount)
        val transferAmount: Long = 5432
        bank.transfer(1, 2, transferAmount)
        assertEquals(depositAmount - transferAmount, bank.getAmount(1))
        assertEquals(transferAmount, bank.getAmount(2))
        assertEquals(depositAmount.toLong(), bank.totalAmount)
    }
}