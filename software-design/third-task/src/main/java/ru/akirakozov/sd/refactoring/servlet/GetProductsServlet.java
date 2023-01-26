package ru.akirakozov.sd.refactoring.servlet;

import ru.akirakozov.sd.refactoring.dao.ProductDao;
import ru.akirakozov.sd.refactoring.response.ProductResponseBuilder;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.Statement;

public class GetProductsServlet extends HttpServlet {

    private final ProductDao productDao;
    private final ProductResponseBuilder<String> responseBuilder;

    public GetProductsServlet(final ProductDao productDao, final ProductResponseBuilder<String> responseBuilder) {
        this.productDao = productDao;
        this.responseBuilder = responseBuilder;
    }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) {
        try {
            final var products = productDao.getAll();
            response.getWriter().println(responseBuilder.buildProductsListResponse(products));
            response.setContentType(responseBuilder.contentType());
            response.setStatus(HttpServletResponse.SC_OK);
        } catch (final Exception e) {
            response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        }
    }
}