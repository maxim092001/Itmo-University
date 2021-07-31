package info.kgeorgiy.ja.grankin.rmi.person;

import java.rmi.RemoteException;

public abstract class AbstractPerson implements Person {
    protected String firstName;
    protected String lastName;
    protected String idNumber;

    public AbstractPerson(final String firstName, final String lastName, final String idNumber) {
        this.firstName = firstName;
        this.lastName = lastName;
        this.idNumber = idNumber;
    }

    @Override
    public String getFirstName() throws RemoteException {
        return this.firstName;
    }

    @Override
    public String getLastName() throws RemoteException {
        return this.lastName;
    }

    @Override
    public String getIdNumber() throws RemoteException {
        return this.idNumber;
    }
}
