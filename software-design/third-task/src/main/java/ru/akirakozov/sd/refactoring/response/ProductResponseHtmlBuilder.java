package ru.akirakozov.sd.refactoring.response;

import ru.akirakozov.sd.refactoring.domain.Product;

import java.util.List;
import java.util.stream.Collectors;

import static ru.akirakozov.sd.refactoring.util.HtmlUtils.wrapHtml;


public class ProductResponseHtmlBuilder implements ProductResponseBuilder<String> {

    public String contentType() {
        return "text/html";
    }

    @Override
    public String buildProductsListResponse(final List<Product> products) {
        return wrapHtml(
                products.stream()
                        .map(p -> String.format("%s\t%d</br>", p.getName(), p.getPrice()))
                        .collect(Collectors.joining("\n"))
        );
    }

    @Override
    public String buildAddProductResponse() {
        return "OK";
    }

    @Override
    public String buildProductWithMinPriceResponse(final Product product) {
        return wrapHtml(
                String.format("<h1>Product with min price: </h1> {%s}\t{%d}</br>", product.getName(), product.getPrice())
        );
    }

    @Override
    public String buildProductWithMaxPriceResponse(final Product product) {
        return wrapHtml(
                String.format("<h1>Product with max price: </h1> {%s}\t{%d}</br>", product.getName(), product.getPrice())
        );
    }

    @Override
    public String buildTotalPriceResponse(final long price) {
        return wrapHtml(
                String.format("Total price: %d", price)
        );
    }

    @Override
    public String buildTotalCountResponse(final int count) {
        return wrapHtml(
                String.format("Number of products: {%d}", count)
        );
    }

    @Override
    public String buildUnknownCommandResponse(final String command) {
        return wrapHtml(String.format("ERROR: unknown command: {%s}", command));
    }
}
