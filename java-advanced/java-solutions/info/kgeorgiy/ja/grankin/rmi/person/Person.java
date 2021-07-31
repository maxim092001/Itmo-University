package info.kgeorgiy.ja.grankin.rmi.person;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface Person extends Remote {
    String getFirstName() throws RemoteException;
    String getLastName() throws RemoteException;
    String getIdNumber() throws RemoteException;

    enum PersonType {
        LOCAL,
        REMOTE
    }
}
