package org.mathoptimization.math_optimization.controllers;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

@RestControllerAdvice
public class ControllersExceptionHandler {
    @ExceptionHandler(Exception.class)
    public ResponseEntity<String> defaultHandle(Exception e) {
        e.printStackTrace(System.err);
        return ResponseEntity.badRequest().body("Error");
    }
}
