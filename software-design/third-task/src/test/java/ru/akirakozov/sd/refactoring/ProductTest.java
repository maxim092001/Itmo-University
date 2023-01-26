package ru.akirakozov.sd.refactoring;

import org.junit.Test;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.Map;
import java.util.stream.Collectors;

import static ru.akirakozov.sd.refactoring.TestUtils.*;
import static ru.akirakozov.sd.refactoring.util.HtmlUtils.*;

public class ProductTest extends BaseTest {

    private static final Map<String, Long> products = Map.of(
            "product1", 100L,
            "product2", 200L,
            "product3", 300L
    );

    @Test
    public void addProductOkTest() throws URISyntaxException, IOException {
        final var expected = "OK";
        final var actual = sendAddProductRequest("test_product", 1);
        assertEqualsIgnoreWhitespace(expected, actual);
    }

    @Test
    public void getEmptyProductsTest() throws URISyntaxException, IOException {
        final var expected = wrapHtml("");
        final var actual = sendGetProductsRequest();
        assertEqualsIgnoreWhitespace(expected, actual);
    }

    @Test
    public void addAndGetProductsTest() throws URISyntaxException, IOException {

        for (final Map.Entry<String, Long> e : products.entrySet()) {
            sendAddProductRequest(e.getKey(), e.getValue());
        }

        final var rawExpectedResult =
                products.entrySet().stream().map(e -> String.format("%s %d</br>",e.getKey(), e.getValue())).collect(Collectors.joining());
        final var expected = wrapHtml(rawExpectedResult);
        final var actual = sendGetProductsRequest();
        assertEqualsIgnoreWhitespace(expected, actual);
    }

}
