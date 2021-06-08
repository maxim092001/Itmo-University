package org.mathoptimization.math_optimization.controllers;

import org.mathoptimization.math_optimization.dto.OptimizationResult;
import org.mathoptimization.math_optimization.services.MethodsService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
public class MethodsController {

    private final MethodsService methodsService;

    @Autowired
    public MethodsController(final MethodsService methodsService) {
        this.methodsService = methodsService;
    }


    @RequestMapping(path = "/calculate", method = RequestMethod.POST)
    public ResponseEntity<OptimizationResult> calculate(@RequestHeader("method") final String method,
                                                        @RequestBody final String json) throws MethodsService.MethodsServiceException {
        return ResponseEntity.ok(methodsService.optimizationMethod(method, json));
    }
}
