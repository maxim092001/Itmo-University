package info.kgeorgiy.ja.grankin.rmi.account;

public abstract class AbstractAccount implements Account {
    protected final String id;
    protected int amount;

    public AbstractAccount(final String id, final int amount) {
        this.id = id;
        this.amount = amount;
    }

    public String getId() {
        return id;
    }

    public synchronized void addAmount(final int amount) {
        setAmount(getAmount() + amount);
    }

    public synchronized int getAmount() {
        System.out.println("Getting amount of money for account " + id);
        return amount;
    }

    public synchronized void setAmount(final int amount) {
        System.out.println("Setting amount of money for account " + id);
        this.amount = amount;
    }
}
