package lab2;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpServer;
import lab2.gradient.methods.ConjugateGradientMinimizer;
import lab2.gradient.methods.GradientDescentFastestMinimizer;
import lab2.gradient.methods.GradientDescentMinimizer;
import lab2.gradient.methods.IterationStep;
import lab2.gradient.utils.MinimizationResult;
import lab2.interop.InteropRequest;

import java.io.IOException;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.util.List;
import java.util.stream.Collectors;

public class Main {
    private static final int PORT = 9991;

    public static void main(String[] args) throws IOException {
        HttpServer httpServer = HttpServer.create(new InetSocketAddress(PORT), 0);
        httpServer.createContext("/", new Handler());
        httpServer.setExecutor(null);
        httpServer.start();
    }

    public static class Handler implements HttpHandler {
        private static final ObjectMapper mapper = new ObjectMapper();

        private void internalHandle(HttpExchange exchange) throws IOException {
            InteropRequest request = mapper.readValue(exchange.getRequestBody(), InteropRequest.class);

            List<IterationStep> steps;

            if ("GradientDescent".equals(request.getMethod())) {
                MinimizationResult result = new GradientDescentMinimizer().hui(
                        request.getParsedFunction(), request.getParsedStartPoint(), request.getEps(), request.getAlpha()
                );
                steps = result.getSteps();
            } else if ("GradientDescentFastest".equals(request.getMethod())) {
                MinimizationResult result = new GradientDescentFastestMinimizer().hui(
                        request.getParsedFunction(), request.getParsedStartPoint(), request.getEps(), request.getAlpha()
                );
                steps = result.getSteps();
            } else {
                ConjugateGradientMinimizer minimizer = new ConjugateGradientMinimizer(
                        request.getParsedFunction(), request.getParsedStartPoint(), request.getEps());
                minimizer.minimize();
                steps = minimizer.getSteps();
            }

            byte[] response = mapper.writeValueAsBytes(steps.stream().map(IterationStep::toInteropResponse).collect(Collectors.toList()));
            exchange.sendResponseHeaders(200, response.length);
            try (OutputStream outputStream = exchange.getResponseBody()) {
                outputStream.write(response);
            }
        }

        @Override
        public void handle(HttpExchange exchange) {
            try {
                internalHandle(exchange);
            } catch (Throwable e) {
                e.printStackTrace();
                throw new RuntimeException(e);
            }
        }
    }
}
