package ru.akirakozov.sd.refactoring.servlet;

import ru.akirakozov.sd.refactoring.dao.ProductDao;
import ru.akirakozov.sd.refactoring.domain.Product;
import ru.akirakozov.sd.refactoring.response.ProductResponseBuilder;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

public class AddProductServlet extends HttpServlet {

    private final ProductDao productDao;
    private final ProductResponseBuilder<String> responseBuilder;

    public AddProductServlet(final ProductDao productDao, final ProductResponseBuilder<String> responseBuilder) {
        this.productDao = productDao;
        this.responseBuilder = responseBuilder;
    }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
        final String name = request.getParameter("name");
        final long price = Long.parseLong(request.getParameter("price"));
        productDao.addProduct(new Product(name, price));

        response.setContentType(responseBuilder.contentType());
        response.setStatus(HttpServletResponse.SC_OK);
        response.getWriter().println(responseBuilder.buildAddProductResponse());
    }
}