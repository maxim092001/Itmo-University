import java.lang.reflect.Modifier
import java.util.concurrent.locks.ReentrantLock
import kotlin.test.*

class CodeTest {
    private val implClass = BankImpl::class.java
    private val accountClass = BankImpl.Account::class.java
    private val accountArrayClass = Array<BankImpl.Account>::class.java
    private val reentrantLockClass = ReentrantLock::class.java

    @Test
    fun testNoSynchronizedMethods() {
        for (m in implClass.declaredMethods) {
            assertFalse((m.modifiers and Modifier.SYNCHRONIZED) != 0, "BankImpl cannot use 'synchronized' methods")
        }
    }

    @Test
    fun testImplFields() {
        val f = implClass.declaredFields
        assertEquals(1, f.size, "BankImpl must have one field")
        assertEquals("accounts", f[0].name, "BankImpl must have accounts field")
        assertEquals(accountArrayClass, f[0].type, "BankImpl must have Account<Accounts> field")
    }

    @Test
    fun testAccountLock() {
        val lockField = accountClass.getDeclaredField("lock")
        assertEquals(reentrantLockClass, lockField.type, "BankImpl.Account.lock field must have ReentrantLock type")
    }

    @Suppress("UNCHECKED_CAST")
    @Test
    fun testFineGrainedLock() {
        val n = 7
        val bank = BankImpl(n)
        val accounts = implClass.getDeclaredField("accounts").apply { isAccessible = true }.get(bank) as Array<BankImpl.Account>
        val lockField = accountClass.getDeclaredField("lock").apply { isAccessible = true }
        for ((i, account) in accounts.withIndex()) {
            lockField.set(account, LoggedLock(i))
        }
        for (i in 0 until n) {
            lockLog.clear()
            bank.deposit(i, 100)
            assertEquals(listOf("lock($i)", "unlock($i)"), lockLog, "Expected lock and unlock on deposit($i, ...)")
        }
        for (i in 0 until n) {
            lockLog.clear()
            bank.withdraw(i, 100)
            assertEquals(listOf("lock($i)", "unlock($i)"), lockLog, "Expected lock and unlock on withdraw($i, ...)")
        }
    }

    private val lockLog = ArrayList<String>()

    private inner class LoggedLock(val i: Int) : ReentrantLock() {
        override fun lock() {
            lockLog += "lock($i)"
            super.lock()
        }

        override fun unlock() {
            lockLog += "unlock($i)"
            super.unlock()
        }
    }
}