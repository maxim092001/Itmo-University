package org.mathoptimization.math_optimization.services;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.util.JSONPObject;
import lombok.extern.slf4j.Slf4j;
import org.mathoptimization.math_optimization.dto.OptimizationResult;
import org.mathoptimization.math_optimization.exception.ApiJsonException;
import org.mathoptimization.math_optimization.exception.ApiMethodException;
import org.mathoptimization.math_optimization.methods.*;
import org.mathoptimization.math_optimization.parameters.Parameters;
import org.springframework.boot.jackson.JsonObjectDeserializer;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Map;

@Service
@Slf4j
public class MethodsService {
    private final FunctionService functionService;

    public MethodsService(FunctionService functionService) {
        this.functionService = functionService;
    }

    /**
     * Function for choosing and calculating optimization.
     *
     * @param method - method name.
     * @param json   - parameters for method.
     * @return - list of parameters used for "logging" our method steps.
     */
    public OptimizationResult optimizationMethod(final String method, final String json) throws MethodsServiceException {
        ObjectMapper mapper = new ObjectMapper();
        var cl = switch (method) {
            case "goldenRatio" -> GoldenRatioMethod.class;
            case "fibonacci" -> FibonacciMethod.class;
            case "dichotomy" -> DichotomyMethod.class;
            case "parabola" -> ParabolaMethod.class;
            case "brent" -> BrentsMethod.class;
            default -> throw new MethodsServiceException("На текущий момент данный метод не поддерживается.");
        };
        try {
            AbstractOptimizationMethod optimizationMethod = mapper.readValue(json, cl);
            optimizationMethod.setFunction(functionService.parse(
                    (String) mapper.readValue(json, new TypeReference<Map<String, Object>>() {
                    }).get("function")
            ));
            optimizationMethod.calculate();
            return new OptimizationResult(
                    optimizationMethod.getParameters(),
                    optimizationMethod.getMinimumValue(),
                    optimizationMethod.getMinArgument()
            );
        } catch (Exception e) {
            throw new MethodsServiceException(e);
        }
    }

    public static class MethodsServiceException extends Exception {
        public MethodsServiceException(Throwable cause) {
            super(cause);
        }

        public MethodsServiceException(String message) {
            super(message);
        }
    }
}
