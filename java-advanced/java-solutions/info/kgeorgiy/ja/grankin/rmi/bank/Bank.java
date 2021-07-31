package info.kgeorgiy.ja.grankin.rmi.bank;

import info.kgeorgiy.ja.grankin.rmi.account.Account;
import info.kgeorgiy.ja.grankin.rmi.person.Person;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.util.Set;

public interface Bank extends Remote {
    Account createAccount(final String id, final Person person) throws RemoteException;

    Account getAccount(final String id, final Person person, final Person.PersonType personType) throws RemoteException;

    Person getPersonById(final String id, final Person.PersonType desiredType) throws RemoteException;

    boolean personExists(final String id) throws RemoteException;

    Set<Account> getPersonsAccounts(final Person person, final Person.PersonType personType) throws RemoteException;

    Set<String> getPersonsAccountsIds(final Person person, final Person.PersonType personType) throws RemoteException;

    Person createPerson(final String firstName, final String lastName, final String idNumber, final Person.PersonType personType) throws RemoteException;
}
