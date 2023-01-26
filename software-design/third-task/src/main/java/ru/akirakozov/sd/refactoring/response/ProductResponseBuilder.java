package ru.akirakozov.sd.refactoring.response;

import ru.akirakozov.sd.refactoring.domain.Product;

import java.util.List;

public interface ProductResponseBuilder<T> {

    String contentType();

    T buildProductsListResponse(final List<Product> products);

    T buildAddProductResponse();

    T buildProductWithMinPriceResponse(final Product product);

    T buildProductWithMaxPriceResponse(final Product product);

    T buildTotalPriceResponse(final long price);

    T buildTotalCountResponse(final int count);

    T buildUnknownCommandResponse(String command);
}
