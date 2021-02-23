package org.mathoptimization.math_optimization.controller;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.mathoptimization.math_optimization.exception.ApiJsonException;
import org.mathoptimization.math_optimization.exception.ApiMethodException;
import org.mathoptimization.math_optimization.methods.*;
import org.mathoptimization.math_optimization.parameters.Parameters;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class MethodsService {


    public List<Parameters> calculate(final String method, final String json) {

        ObjectMapper mapper = new ObjectMapper();
        try {
            var cl = switch (method) {
                case "golden-ratio" -> GoldenRatioMethod.class;
                case "fibonacci" -> FibonacciMethod.class;
                case "dichotomy" -> DichotomyMethod.class;
                case "parabola" -> ParabolaMethod.class;
                default -> throw new ApiMethodException("На текущий момент данный метод не поддерживается.");
            };
            AbstractOptimizationMethod optimizationMethod = mapper.readValue(json, cl);
            optimizationMethod.setFunction(x -> Math.pow(Math.log10(x - 2), 2.0) + Math.pow(Math.log10(10 - x), 2.0) - Math.pow(x, 0.2));
            optimizationMethod.calculate();
            return optimizationMethod.getParameters();
        } catch (JsonProcessingException e) {
            throw new ApiJsonException("Что-то пошло не так.");
        }
    }

}
