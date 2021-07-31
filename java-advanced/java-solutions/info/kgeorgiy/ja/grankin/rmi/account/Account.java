package info.kgeorgiy.ja.grankin.rmi.account;

import info.kgeorgiy.ja.grankin.rmi.person.Person;

import java.rmi.*;

public interface Account extends Remote {
    /**
     * Returns account identifier.
     */
    String getId() throws RemoteException;

    /**
     * Returns amount of money at the account.
     */
    int getAmount() throws RemoteException;

    /**
     * Sets amount of money at the account.
     */
    void setAmount(int amount) throws RemoteException;

    void addAmount(final int amount) throws RemoteException;


    static String toAccountId(final Person person, final String id) throws RemoteException {
        return String.format("%s:%s", person.getIdNumber(), id);
    }
}