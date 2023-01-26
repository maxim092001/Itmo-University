package ru.akirakozov.sd.refactoring;

import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.util.EntityUtils;
import org.junit.Assert;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.stream.Collectors;

public class TestUtils {

    public static String sendGetRequest(final String method, final Map<String, Object> params) throws IOException, URISyntaxException {
        try (var httpClient = HttpClients.createDefault()) {
            var httpGet = new HttpGet("http://0.0.0.0:" + BaseTest.appConfig.getServerPort() + "/" + method);
            var uriBuilder = new URIBuilder(httpGet.getURI());

            uriBuilder.addParameters(
                    params.entrySet().stream()
                            .map(e ->
                                    new BasicNameValuePair(e.getKey(), e.getValue().toString()
                                    )
                            )
                            .collect(Collectors.toList()));
            var uri = uriBuilder.build();
            httpGet.setURI(uri);

            try (var response = httpClient.execute(httpGet)) {
                return EntityUtils.toString(response.getEntity(), StandardCharsets.UTF_8);
            }
        }
    }

    public static String sendGetProductsRequest() throws URISyntaxException, IOException {
        return sendGetRequest("get-products", Map.of());
    }

    public static String sendAddProductRequest(final String name, final long price) throws URISyntaxException, IOException {
        return sendGetRequest("add-product", Map.of(
                "name", name,
                "price", price
        ));
    }


    public static void assertEqualsIgnoreWhitespace(final String expected, final String actual) {
        final String s1 = expected.replaceAll("\\s+", "");
        final String s2 = actual.replaceAll("\\s+", "");
        Assert.assertEquals(s1, s2);
    }
}
