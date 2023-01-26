package ru.akirakozov.sd.refactoring.connection;

import ru.akirakozov.sd.refactoring.config.DatabaseConfig;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

public class JdbcConnectionProvider implements ConnectionProvider {

    private final DatabaseConfig config;

    public JdbcConnectionProvider(final DatabaseConfig config) {
        this.config = config;
    }

    @Override
    public Connection getConnection() throws SQLException {
        return DriverManager.getConnection(config.getUrl());
    }

}
