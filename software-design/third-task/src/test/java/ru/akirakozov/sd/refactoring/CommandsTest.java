package ru.akirakozov.sd.refactoring;

import org.junit.Test;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.Map;

import static ru.akirakozov.sd.refactoring.TestUtils.*;
import static ru.akirakozov.sd.refactoring.util.HtmlUtils.wrapHtml;

public class CommandsTest extends BaseTest {

    private static final Map<String, Long> products = Map.of(
            "product1", 100L,
            "product2", 200L,
            "product3", 300L
    );

    private static final int count = products.size();
    private static final Map.Entry<String, Long> minPrice = products.entrySet().stream()
            .min(Map.Entry.comparingByValue()).get();
    private static final Map.Entry<String, Long> maxPrice = products.entrySet().stream()
            .max(Map.Entry.comparingByValue()).get();
    private static final Long sum = products.values().stream().reduce(Long::sum).get();

    @Test
    public void testSum() throws URISyntaxException, IOException {
        addSampleProducts();
        var expected = wrapHtml(
                String.format("Total price: %d", sum)
        );
        var actual = sendQueryRequest("sum");
        assertEqualsIgnoreWhitespace(expected, actual);
    }

    @Test
    public void testCount() throws URISyntaxException, IOException {
        addSampleProducts();
        var expected = wrapHtml(
                String.format("Number of products: {%d}", count)
        );
        var actual = sendQueryRequest("count");
        assertEqualsIgnoreWhitespace(expected, actual);
    }

    @Test
    public void testMin() throws URISyntaxException, IOException {
        addSampleProducts();
        var expected = wrapHtml(
                String.format("<h1>Product with min price: </h1> {%s} {%d}</br>", minPrice.getKey(), minPrice.getValue())
        );
        var actual = sendQueryRequest("min");
        assertEqualsIgnoreWhitespace(expected, actual);
    }

    @Test
    public void testMax() throws URISyntaxException, IOException {
        addSampleProducts();
        var expected = wrapHtml(
                String.format("<h1>Product with max price: </h1> {%s} {%d}</br>", maxPrice.getKey(), maxPrice.getValue())
        );
        var actual = sendQueryRequest("max");
        assertEqualsIgnoreWhitespace(expected, actual);
    }

    private void addSampleProducts() throws URISyntaxException, IOException {
        for (Map.Entry<String, Long> e : products.entrySet()) {
            sendAddProductRequest(e.getKey(), e.getValue());
        }
    }

    private String sendQueryRequest(final String command) throws URISyntaxException, IOException {
        return sendGetRequest("query", Map.of("command", command));
    }
}
