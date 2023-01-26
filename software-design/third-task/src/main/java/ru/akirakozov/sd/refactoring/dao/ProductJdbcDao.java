package ru.akirakozov.sd.refactoring.dao;

import ru.akirakozov.sd.refactoring.connection.ConnectionProvider;
import ru.akirakozov.sd.refactoring.domain.Product;
import ru.akirakozov.sd.refactoring.util.FunctionWithException;

import java.io.IOException;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

public class ProductJdbcDao implements ProductDao {

    private final ConnectionProvider connectionProvider;

    public ProductJdbcDao(final ConnectionProvider connectionProvider) {
        this.connectionProvider = connectionProvider;
    }

    @Override
    public List<Product> getAll() throws IOException {
        return query("select * from product;", result -> {
            final List<Product> res = new ArrayList<>();
            while (result.next()) {
                final String name = result.getString("name");
                final long price = result.getLong("price");
                res.add(new Product(name, price));
            }
            return res;
        });
    }

    @Override
    public void addProduct(final Product product) throws IOException {
        try (var connection = connectionProvider.getConnection()) {
            final String preparedSql = "insert into product (name, price) values (?, ?)";
            try (var prepareStatement= connection.prepareStatement(preparedSql)) {
                prepareStatement.setString(1, product.getName());
                prepareStatement.setLong(2, product.getPrice());
                prepareStatement.executeUpdate();
            }
        } catch (SQLException e) {
            throw new IOException("Error while executing sql query", e);
        }
    }

    @Override
    public Product getProductWithMaxPrice() throws IOException {
        return getOneProduct("select name, price from product order by price desc limit 1;");
    }

    @Override
    public Product getProductWithMinPrice() throws IOException {
        return getOneProduct("select name, price from product order by price limit 1;");
    }

    private Product getOneProduct(final String sql) throws IOException {
        return query(sql, result -> {
            Product res = null;
            while (result.next()) {
                final String name = result.getString("name");
                final long price = result.getLong("price");
                res = new Product(name, price);
            }
            return res;
        });
    }


    @Override
    public long getSumPrice() throws IOException {
        return query("select sum(price) from product;", result -> result.getLong(1));
    }


    public int getProductCount() throws IOException {
        return query("select count(*) from product;", result -> result.getInt(1));
    }

    private <T> T query(final String sql, FunctionWithException<ResultSet, T> mapper) throws IOException {
        try (var connection = connectionProvider.getConnection();
             var statement = connection.createStatement();
             var result = statement.executeQuery(sql)
        ) {
            return mapper.apply(result);
        } catch (Exception e) {
            throw new IOException("Error during sql query", e);
        }
    }
}
