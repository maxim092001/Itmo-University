package ru.akirakozov.sd.refactoring;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import ru.akirakozov.sd.refactoring.config.AppConfig;
import ru.akirakozov.sd.refactoring.config.DatabaseConfig;
import ru.akirakozov.sd.refactoring.config.ServerConfig;

import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public abstract class BaseTest {

    private static final ExecutorService es = Executors.newSingleThreadExecutor();
    static final AppConfig appConfig = new AppConfig(new DatabaseConfig("jdbc:sqlite:product-test.db"), new ServerConfig(8081));

    @BeforeClass
    public static void startServer() {
        es.submit(() -> {
            try {
                Main.startServer(appConfig);
            } catch (Throwable e) {
                System.err.println("Server failed during test: ");
                e.printStackTrace();
            }
        });
    }

    @AfterClass
    public static void shutdownServer() {
        es.shutdown();
    }

    @After
    public void dropProductsTable() {
        try (var connection = DriverManager.getConnection(appConfig.getDatabaseUrl());
             var statement = connection.createStatement()) {
            statement.executeUpdate("delete from product where 1 = 1;");
        } catch (SQLException e) {
            System.err.println("Failed to clear 'product' table after test execution: ");
            e.printStackTrace();
        }
    }

}
