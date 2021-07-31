package info.kgeorgiy.ja.grankin.rmi;

import info.kgeorgiy.ja.grankin.rmi.account.Account;
import info.kgeorgiy.ja.grankin.rmi.bank.Bank;
import info.kgeorgiy.ja.grankin.rmi.bank.RemoteBank;
import info.kgeorgiy.ja.grankin.rmi.person.AbstractPerson;
import info.kgeorgiy.ja.grankin.rmi.person.Person;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.internal.TextListener;
import org.junit.runner.JUnitCore;

import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;
import java.util.Objects;
import java.util.concurrent.Callable;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

public class BankTests {

    private static Bank bank;
    private static final int ITERATIONS = 100;
    private static final int REGISTRY_PORT = 8889;
    private static final int BANK_PORT = 8080;
    private static final int THREADS = 10;
    private static final int JOBS = 20;
    private static final String FIRST_NAME = "firstName";
    private static final String LAST_NAME = "lastName";
    private static final String ID_NUMBER = "idNumber";
    private static final String ACCOUNT_ID = "accountId";
    private static final String AMOUNT = "100";
    private static final String EMPTY = "";

    public static void main(String[] args) {
        JUnitCore junit = new JUnitCore();
        junit.addListener(new TextListener(System.out));
        junit.run(BankTests.class);
        System.exit(0);
    }

    @BeforeClass
    public static void beforeClass() throws RemoteException, NotBoundException {
        final Registry registry = LocateRegistry.createRegistry(REGISTRY_PORT);
        registry.rebind("//localhost/bank", new RemoteBank(BANK_PORT));
        bank = (Bank) registry.lookup("//localhost/bank");
        UnicastRemoteObject.exportObject(bank, REGISTRY_PORT);
    }


    @Test
    public void getNonExistingPerson() throws RemoteException {
        assertNull(bank.getPersonById("stupid-id-remote", Person.PersonType.REMOTE));
        assertNull(bank.getPersonById("stupid-id-local", Person.PersonType.LOCAL));
    }

    @Test
    public void createOneAccount() throws RemoteException {
        createSimplePerson(Person.PersonType.REMOTE, EMPTY, EMPTY, "1");
        var person = bank.getPersonById(ID_NUMBER + "1", Person.PersonType.REMOTE);
        var account = createEmptyAccount(person);
        assertEquals(account.getId(), ACCOUNT_ID);
        assertEquals(account.getAmount(), 0);
        account.setAmount(100);
        assertEquals(account.getAmount(), 100);
        assertEquals(bank.getPersonsAccounts(person, Person.PersonType.REMOTE).size(), 1);
    }

    @Test
    public void personWithoutAccounts() throws RemoteException {
        var person = createSimplePerson(Person.PersonType.LOCAL, EMPTY, EMPTY, "empty");
        assertEquals(bank.getPersonsAccounts(person, Person.PersonType.LOCAL).size(), 0);
    }

    @Test
    public void createOnePerson() throws RemoteException {
        createPersons(1);
    }

    @Test
    public void createOneHundredPersons() throws RemoteException {
        createPersons(ITERATIONS);
    }

    @Test
    public void checkClient() throws RemoteException {
        Client.main(FIRST_NAME, LAST_NAME, ID_NUMBER, ACCOUNT_ID, AMOUNT, Integer.toString(REGISTRY_PORT));
        var person = bank.getPersonById(ID_NUMBER, Person.PersonType.REMOTE);
        var account = bank.getAccount(ACCOUNT_ID, person, Person.PersonType.REMOTE);
        comparePersons(new TestPerson(FIRST_NAME, LAST_NAME, ID_NUMBER), person);
        assertEquals(100, account.getAmount());
    }

    private void createPersons(final int n) throws RemoteException {
        final String idNumber = ID_NUMBER;
        for (int i = 0; i < n; i++) {
            var remote = createSimplePerson(Person.PersonType.REMOTE, EMPTY, EMPTY, Integer.toString(i));
            var local = createSimplePerson(Person.PersonType.LOCAL, EMPTY, EMPTY, Integer.toString(i));
            comparePersons(remote, bank.getPersonById(idNumber + i, Person.PersonType.REMOTE));
            comparePersons(local, bank.getPersonById(idNumber + i, Person.PersonType.LOCAL));
        }
    }

    private Person createSimplePerson(final Person.PersonType personType, final String... args) throws RemoteException {
        var firstName = FIRST_NAME;
        var lastName = LAST_NAME;
        var idNumber = ID_NUMBER;
        if (Objects.nonNull(args)) {
            if (args.length > 0 && args[0] != null) {
                firstName += args[0];
            }
            if (args.length > 1 && args[1] != null) {
                lastName += args[1];
            }
            if (args.length > 2 && args[2] != null) {
                idNumber += args[2];
            }
        }
        return bank.createPerson(firstName, lastName, idNumber, personType);
    }

    private Account createEmptyAccount(final Person person) throws RemoteException {
        return bank.createAccount(ACCOUNT_ID, person);
    }

    @Test
    public void concurrentAmount() throws RemoteException, InterruptedException {
        createSimplePerson(Person.PersonType.REMOTE, EMPTY, EMPTY, "1");
        final var account = createEmptyAccount(bank.getPersonById(ID_NUMBER + "1", Person.PersonType.REMOTE));
        final var added = new AtomicInteger();
        final var executorService = Executors.newFixedThreadPool(THREADS);
        executorService.invokeAll(
                IntStream.rangeClosed(1, JOBS)
                        .<Callable<Integer>>mapToObj(mn -> () -> {
                            account.addAmount(mn);
                            added.addAndGet(mn);
                            return account.getAmount();
                        }).collect(Collectors.toList()));
        assertEquals(added.get(), account.getAmount());
    }


    private void comparePersons(
            final Person first,
            final Person second
    ) throws RemoteException {
        assertEquals(first.getFirstName(), second.getFirstName());
        assertEquals(first.getLastName(), second.getLastName());
        assertEquals(first.getIdNumber(), second.getIdNumber());
    }

    private static class TestPerson extends AbstractPerson {

        public TestPerson(final String firstName, final String lastName, final String idNumber) {
            super(firstName, lastName, idNumber);
        }
    }
}
