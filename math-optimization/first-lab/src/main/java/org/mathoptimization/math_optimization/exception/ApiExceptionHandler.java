package org.mathoptimization.math_optimization.exception;

import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

import java.time.ZoneId;
import java.time.ZonedDateTime;

/**
 * @author Grankin Maxim (maximgran@gmail.com) at 17:32 04.06.2020
 */
@Slf4j
@ControllerAdvice
public class ApiExceptionHandler {

    @ExceptionHandler(value = {ApiJsonException.class})
    public ResponseEntity<Object> handleBadRequest(ApiJsonException e) {
        final HttpStatus badRequest = HttpStatus.BAD_REQUEST;
        ApiException apiException = new ApiException(
                e.getMessage(),
                badRequest,
                ZonedDateTime.now(ZoneId.of("Z"))
        );
        log.error("Bad request: " + e.getMessage());
        return new ResponseEntity<>(apiException, badRequest);
    }

    @ExceptionHandler(value = {ApiMethodException.class})
    public ResponseEntity<Object> handleForbidden(ApiMethodException e) {
        final HttpStatus notFound = HttpStatus.NOT_FOUND;
        ApiException apiException = new ApiException(
                e.getMessage(),
                notFound,
                ZonedDateTime.now(ZoneId.of("Z"))
        );
        log.error("Not found: " + e.getMessage());
        return new ResponseEntity<>(apiException, notFound);
    }
}