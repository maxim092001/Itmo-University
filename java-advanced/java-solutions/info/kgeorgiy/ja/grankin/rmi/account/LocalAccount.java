package info.kgeorgiy.ja.grankin.rmi.account;

import java.io.Serializable;

public class LocalAccount extends AbstractAccount implements Serializable {
    public LocalAccount(final String id, final int amount) {
        super(id, amount);
    }
}
