package info.kgeorgiy.ja.grankin.rmi;

import info.kgeorgiy.ja.grankin.rmi.bank.Bank;
import info.kgeorgiy.ja.grankin.rmi.bank.RemoteBank;

import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;

public final class Server {
    private final static int DEFAULT_PORT = 8888;

    public static void main(final String... args) throws RemoteException {
        final int port = args.length > 0 ? Integer.parseInt(args[0]) : DEFAULT_PORT;

        final Bank bank = new RemoteBank(port);
        try {
            Registry registry = LocateRegistry.createRegistry(DEFAULT_PORT);
            registry.rebind("//localhost/bank", bank);
            UnicastRemoteObject.exportObject(bank, port);
            System.out.println("Server started");
        } catch (final RemoteException e) {
            System.out.println("Cannot export object: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }
    }
}
