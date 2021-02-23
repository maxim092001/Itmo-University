package org.mathoptimization.math_optimization.controller;

import org.mathoptimization.math_optimization.parameters.Parameters;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
public class MethodsController {

    private final MethodsService methodsService;

    @Autowired
    public MethodsController(final MethodsService methodsService) {
        this.methodsService = methodsService;
    }


    @RequestMapping(path = "/calculate", method = RequestMethod.POST)
    public ResponseEntity<List<Parameters>> calculate(@RequestHeader("method") final String method,
                                                      @RequestBody final String json) {
        return ResponseEntity.ok(methodsService.optimizationMethod(method, json));
    }
}
