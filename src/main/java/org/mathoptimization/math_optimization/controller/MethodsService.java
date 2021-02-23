package org.mathoptimization.math_optimization.controller;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.mathoptimization.math_optimization.exception.ApiJsonException;
import org.mathoptimization.math_optimization.exception.ApiMethodException;
import org.mathoptimization.math_optimization.methods.*;
import org.mathoptimization.math_optimization.parameters.Parameters;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@Slf4j
public class MethodsService {


    /**
     * Function for choosing and calculating optimization.
     *
     * @param method - method name.
     * @param json   - parameters for method.
     * @return - list of parameters used for "logging" our method steps.
     */
    public List<Parameters> optimizationMethod(final String method, final String json) {

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
            log.error(e.getMessage());
            throw new ApiJsonException("Что-то пошло не так.");
        }
    }

}
