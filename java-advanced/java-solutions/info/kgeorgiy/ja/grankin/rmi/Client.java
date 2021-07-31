package info.kgeorgiy.ja.grankin.rmi;

import info.kgeorgiy.ja.grankin.rmi.bank.Bank;
import info.kgeorgiy.ja.grankin.rmi.person.Person;

import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.util.Objects;
import java.util.stream.Stream;

public final class Client {
    private Client() {
    }

    public static void main(final String... args) throws RemoteException {
        final Bank bank;
        try {
            bank = (Bank) LocateRegistry.getRegistry(Objects.nonNull(args) && args.length > 5 ? Integer.parseInt(args[5]) : 8888).lookup("//localhost/bank");
        } catch (final NotBoundException e) {
            System.out.println("Bank is not bound");
            return;
        } catch (final NumberFormatException e) {
            System.out.println("Port have to be integer");
            return;
        }

        if (Stream.of(args).anyMatch(Objects::isNull)) {
            System.err.println("All args have to be non null");
        } else {
            try {
                final String firstName = args[0];
                final String lastName = args[1];
                final String idNumber = args[2];
                final String accountId = args[3];

                final int balance;
                try {
                    balance = Integer.parseInt(args[4]);
                } catch (final NumberFormatException e) {
                    System.err.println("Balance have to be integer");
                    return;
                }
                final Person person;
                if (bank.personExists(idNumber)) {
                    person = bank.getPersonById(idNumber, Person.PersonType.REMOTE);
                } else {
                    System.out.printf("Creating new person with id: %s%n", idNumber);
                    person = bank.createPerson(firstName, lastName, idNumber, Person.PersonType.REMOTE);
                }

                if (!bank.getPersonsAccountsIds(person, Person.PersonType.REMOTE).contains(accountId)) {
                    final var account = bank.getAccount(accountId, person, Person.PersonType.REMOTE);
                    if (Objects.nonNull(account)) {
                        System.err.println("Account already exists");
                        return;
                    }
                    System.out.printf("Creating account for person with id: %s%n", idNumber);
                    bank.createAccount(accountId, person);
                }
                final var account = bank.getAccount(accountId, person, Person.PersonType.REMOTE);

                System.out.println("Account id: " + account.getId());
                System.out.println("Money: " + account.getAmount());
                System.out.println("Adding money");
                account.setAmount(account.getAmount() + balance);
                System.out.println("Money: " + account.getAmount());
            } catch (final RemoteException e) {
                System.err.printf("Remote exception %s%n", e.getLocalizedMessage());
            }
        }
    }
}
