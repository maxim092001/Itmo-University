package ru.akirakozov.sd.refactoring.dao;

import ru.akirakozov.sd.refactoring.domain.Product;

import java.io.IOException;
import java.util.List;

public interface ProductDao {

    List<Product> getAll() throws IOException;

    void addProduct(Product product) throws IOException;

    Product getProductWithMaxPrice() throws IOException;

    Product getProductWithMinPrice() throws IOException;

    long getSumPrice() throws IOException;

    int getProductCount() throws IOException;
}
