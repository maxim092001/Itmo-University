import kotlin.test.*

/**
 * Functional single-threaded test-suite for bank implementation.
 */
class FunctionalTest {
    private val n = 10
    private val bank: Bank = BankImpl(n)

    @Test
    fun testEmptyBank() {
        assertEquals(n, bank.numberOfAccounts)
        assertEquals(0, bank.totalAmount)
        for (i in 0 until n) assertEquals(0, bank.getAmount(i))
    }

    @Test
    fun testDeposit() {
        val amount = 1234L
        val result = bank.deposit(1, amount)
        assertEquals(amount, result)
        assertEquals(amount, bank.getAmount(1))
        assertEquals(amount, bank.totalAmount)
    }

    @Test
    fun testWithdraw() {
        val depositAmount = 2345L
        val depositResult = bank.deposit(1, depositAmount)
        assertEquals(depositAmount, depositResult)
        assertEquals(depositAmount, bank.getAmount(1))
        assertEquals(depositAmount, bank.totalAmount)
        val withdrawAmount = 1234L
        val withdrawResult = bank.withdraw(1, withdrawAmount)
        assertEquals(depositAmount - withdrawAmount, withdrawResult)
        assertEquals(depositAmount - withdrawAmount, bank.getAmount(1))
        assertEquals(depositAmount - withdrawAmount, bank.totalAmount)
    }

    @Test
    fun testTotalAmount() {
        val deposit1 = 4567L
        val depositResult1 = bank.deposit(1, deposit1)
        assertEquals(deposit1, depositResult1)
        assertEquals(deposit1, bank.totalAmount)
        val deposit2 = 6789L
        val depositResult2 = bank.deposit(2, deposit2)
        assertEquals(deposit2, depositResult2)
        assertEquals(deposit2, bank.getAmount(2))
        assertEquals(deposit1 + deposit2, bank.totalAmount)
    }

    @Test
    fun testTransfer() {
        val depositAmount = 9876L
        val depositResult = bank.deposit(1, depositAmount)
        assertEquals(depositAmount, depositResult)
        assertEquals(depositAmount, bank.getAmount(1))
        assertEquals(depositAmount, bank.totalAmount)
        val transferAmount = 5432L
        bank.transfer(1, 2, transferAmount)
        assertEquals(depositAmount - transferAmount, bank.getAmount(1))
        assertEquals(transferAmount, bank.getAmount(2))
        assertEquals(depositAmount, bank.totalAmount)
    }
}