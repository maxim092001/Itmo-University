package ru.akirakozov.sd.refactoring.servlet;

import ru.akirakozov.sd.refactoring.dao.ProductDao;
import ru.akirakozov.sd.refactoring.response.ProductResponseBuilder;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

public class QueryServlet extends HttpServlet {

    private final ProductDao productDao;
    private final ProductResponseBuilder<String> responseBuilder;

    public QueryServlet(final ProductDao productDao, final ProductResponseBuilder<String> responseBuilder) {
        this.productDao = productDao;
        this.responseBuilder = responseBuilder;
    }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
        final String command = request.getParameter("command");

        switch (command) {
            case "max":
                response.getWriter().println(responseBuilder.buildProductWithMaxPriceResponse(productDao.getProductWithMaxPrice()));
                break;
            case "min":
                response.getWriter().println(responseBuilder.buildProductWithMinPriceResponse(productDao.getProductWithMinPrice()));
                break;
            case "count":
                final int count = productDao.getProductCount();
                response.getWriter().println(responseBuilder.buildTotalCountResponse(count));
                break;
            case "sum":
                final long price = productDao.getSumPrice();
                response.getWriter().println(responseBuilder.buildTotalPriceResponse(price));
                break;
            default:
                response.getWriter().println(responseBuilder.buildUnknownCommandResponse(command));
                break;
        }
        response.setContentType(responseBuilder.contentType());
        response.setStatus(HttpServletResponse.SC_OK);
    }
}