package info.kgeorgiy.ja.grankin.rmi.person;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

public class RemotePerson extends AbstractPerson {
    public RemotePerson(
            final String firstName,
            final String lastName,
            final String idNumber,
            final int port
    ) throws RemoteException {
        super(firstName, lastName, idNumber);
        UnicastRemoteObject.exportObject(this, port);
    }
}
