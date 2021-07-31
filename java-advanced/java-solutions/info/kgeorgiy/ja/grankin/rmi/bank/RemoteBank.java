package info.kgeorgiy.ja.grankin.rmi.bank;

import info.kgeorgiy.ja.grankin.rmi.account.Account;
import info.kgeorgiy.ja.grankin.rmi.account.LocalAccount;
import info.kgeorgiy.ja.grankin.rmi.account.RemoteAccount;
import info.kgeorgiy.ja.grankin.rmi.person.LocalPerson;
import info.kgeorgiy.ja.grankin.rmi.person.Person;
import info.kgeorgiy.ja.grankin.rmi.person.RemotePerson;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.stream.Collectors;

public class RemoteBank implements Bank {
    private final int port;
    private final ConcurrentMap<String, Account> accountById = new ConcurrentHashMap<>();
    private final ConcurrentMap<String, Person> personById = new ConcurrentHashMap<>();
    private final ConcurrentMap<String, Set<Account>> accountsById = new ConcurrentHashMap<>();

    public RemoteBank(final int port) {
        this.port = port;
    }

    @Override
    public Account createAccount(final String id, final Person person) throws RemoteException {
        if (Objects.isNull(person) || Objects.isNull(id)) {
            return null;
        }
        final var accountId = Account.toAccountId(person, id);
        return accountById.getOrDefault(accountId, createNewAccount(id, accountId, person));
    }

    private Account createNewAccount(final String id, final String accountId, final Person person) throws RemoteException {
        var account = new RemoteAccount(id);
        UnicastRemoteObject.exportObject(account, port);
        accountById.put(accountId, account);
        if (!accountsById.containsKey(person.getIdNumber())) {
            accountsById.put(person.getIdNumber(), Collections.newSetFromMap(new ConcurrentHashMap<>()));
        }
        accountsById.get(person.getIdNumber()).add(account);
        return account;
    }

    @Override
    public Account getAccount(final String id, final Person person, final Person.PersonType personType) throws RemoteException {

        if (List.of(id, person, personType).stream().anyMatch(Objects::isNull)) {
            return null;
        }

        return switch (personType) {
            case REMOTE -> accountById.getOrDefault(Account.toAccountId(person, id), null);
            case LOCAL -> ((LocalPerson) person).getAccountById(id);
        };
    }

    @Override
    public Person getPersonById(final String id, final Person.PersonType desiredType) throws RemoteException {
        var optP = Optional.ofNullable(id).map(personById::get);
        return switch (desiredType) {
            case REMOTE -> optP.orElse(null);
            case LOCAL -> {
                if (optP.isPresent()) {
                    final var p = optP.get();
                    final Map<String, Account> accountsMap = new ConcurrentHashMap<>();
                    for (Account account : getPersonsAccounts(p, desiredType)) {
                        accountsMap.put(account.getId(), account);
                    }
                    yield new LocalPerson(
                            p.getFirstName(),
                            p.getLastName(),
                            p.getIdNumber(),
                            accountsMap);
                } else yield null;
            }
        };
    }

    @Override
    public boolean personExists(final String id) throws RemoteException {
        return Optional.ofNullable(id).map(personById::get).isPresent();
    }

    @Override
    public Set<Account> getPersonsAccounts(final Person person, final Person.PersonType personType) throws RemoteException {
        return switch (personType) {
            case REMOTE -> accountsById.get(person.getIdNumber());
            case LOCAL -> ((LocalPerson) person).getAccounts().stream().map(account -> (LocalAccount) account).collect(Collectors.toSet());
        };
    }

    @Override
    public Set<String> getPersonsAccountsIds(final Person person, final Person.PersonType personType) throws RemoteException {
        final Set<String> s = ConcurrentHashMap.newKeySet();
        for (final Account a : getPersonsAccounts(person, personType)) {
            s.add(a.getId());
        }
        return s;
    }

    @Override
    public Person createPerson(final String firstName, final String lastName, final String idNumber, final Person.PersonType personType) throws RemoteException {
        if (List.of(firstName, lastName, idNumber).stream().anyMatch(Objects::isNull)) {
            return null;
        }
        return personById.getOrDefault(idNumber, createNewPerson(firstName, lastName, idNumber, personType));
    }

    private Person createNewPerson(final String firstName, final String lastName, final String idNumber, final Person.PersonType personType) throws RemoteException {
        return switch (personType) {
            case REMOTE -> {
                final var person = new RemotePerson(firstName, lastName, idNumber, port);
                personById.put(idNumber, person);
                accountsById.put(idNumber, ConcurrentHashMap.newKeySet());
                yield person;
            }
            case LOCAL -> {
                final var person = new LocalPerson(firstName, lastName, idNumber, new ConcurrentHashMap<>());
                personById.put(idNumber, person);
                yield person;
            }
        };
    }
}
