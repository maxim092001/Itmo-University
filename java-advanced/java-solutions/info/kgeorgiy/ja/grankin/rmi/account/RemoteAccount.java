package info.kgeorgiy.ja.grankin.rmi.account;

import java.rmi.RemoteException;

public class RemoteAccount extends AbstractAccount {
    public RemoteAccount(final String id) throws RemoteException {
        super(id, 0);
    }
}
