package ru.akirakozov.sd.refactoring;

import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.servlet.ServletContextHandler;
import org.eclipse.jetty.servlet.ServletHolder;
import ru.akirakozov.sd.refactoring.config.AppConfig;
import ru.akirakozov.sd.refactoring.config.ConfigReader;
import ru.akirakozov.sd.refactoring.connection.ConnectionProvider;
import ru.akirakozov.sd.refactoring.connection.JdbcConnectionProvider;
import ru.akirakozov.sd.refactoring.dao.ProductDao;
import ru.akirakozov.sd.refactoring.dao.ProductJdbcDao;
import ru.akirakozov.sd.refactoring.response.ProductResponseBuilder;
import ru.akirakozov.sd.refactoring.response.ProductResponseHtmlBuilder;
import ru.akirakozov.sd.refactoring.servlet.AddProductServlet;
import ru.akirakozov.sd.refactoring.servlet.GetProductsServlet;
import ru.akirakozov.sd.refactoring.servlet.QueryServlet;

import java.sql.Connection;
import java.sql.Statement;

/**
 * @author akirakozov
 */
public class Main {
    public static void startServer(final AppConfig appConfig) throws Exception {
        final ConnectionProvider connectionProvider = new JdbcConnectionProvider(appConfig.getDatabaseConfig());
        try (Connection c = connectionProvider.getConnection()) {
            final String sql = "CREATE TABLE IF NOT EXISTS PRODUCT" +
                    "(ID INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL," +
                    " NAME           TEXT    NOT NULL, " +
                    " PRICE          INT     NOT NULL)";
            Statement stmt = c.createStatement();

            stmt.executeUpdate(sql);
            stmt.close();
        }

        final Server server = new Server(appConfig.getServerPort());

        final ProductDao productDao = new ProductJdbcDao(connectionProvider);
        final ProductResponseBuilder<String> responseBuilder = new ProductResponseHtmlBuilder();

        ServletContextHandler context = new ServletContextHandler(ServletContextHandler.SESSIONS);
        context.setContextPath("/");
        server.setHandler(context);

        context.addServlet(new ServletHolder(new AddProductServlet(productDao, responseBuilder)), "/add-product");
        context.addServlet(new ServletHolder(new GetProductsServlet(productDao, responseBuilder)), "/get-products");
        context.addServlet(new ServletHolder(new QueryServlet(productDao, responseBuilder)), "/query");

        server.start();
        server.join();
    }

    public static void main(final String[] args) throws Exception {
        if (args.length != 1) {
            System.err.println("Expected only one argument");
            return;
        }

        final String env = args[0];
        final ConfigReader configReader = new ConfigReader("application.conf", "product");

        AppConfig appConfig;
        switch (env) {
            case "test":
            case "prod":
                appConfig = configReader.getAppConfig(env);
               break;
            default:
                System.err.printf("Unknown env: {%s}. Available values: test, prod", env);
                return;
        }
        startServer(appConfig);
    }
}