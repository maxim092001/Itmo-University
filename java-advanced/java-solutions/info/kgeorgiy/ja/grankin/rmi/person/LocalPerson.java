package info.kgeorgiy.ja.grankin.rmi.person;

import info.kgeorgiy.ja.grankin.rmi.account.Account;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class LocalPerson extends AbstractPerson implements Serializable {

    private final Map<String, Account> personsAccounts;

    public LocalPerson(
            final String firstName,
            final String lastName,
            final String idNumber,
            final Map<String, Account> personsAccounts
    ) {
        super(firstName, lastName, idNumber);
        this.personsAccounts = personsAccounts;
    }

    public Set<Account> getAccounts() {
        return new HashSet<>(personsAccounts.values());
    }

    public Set<String> getAccountIds() {
        return personsAccounts.keySet();
    }

    public Account getAccountById(final String accountId) {
        return personsAccounts.getOrDefault(accountId, null);
    }

}
