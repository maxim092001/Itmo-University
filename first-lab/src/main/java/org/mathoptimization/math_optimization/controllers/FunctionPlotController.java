package org.mathoptimization.math_optimization.controllers;

import org.mathoptimization.math_optimization.dto.PointsRequest;
import org.mathoptimization.math_optimization.services.FunctionService;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;
import java.util.function.Function;

@RestController
public class FunctionPlotController {
    private final FunctionService functionService;

    public FunctionPlotController(FunctionService functionService) {
        this.functionService = functionService;
    }

    @RequestMapping(path = "/plot", method = {RequestMethod.POST, RequestMethod.GET}, produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<List<FunctionService.Point>> plotPoints(@RequestBody PointsRequest pointsRequest) {
        Function<Double, Double> function;
        try {
            function = functionService.parse(pointsRequest.getFunction());
            return ResponseEntity.ok().body(functionService.calcPoints(
                    function,
                    pointsRequest.getFrom(),
                    pointsRequest.getTo(),
                    pointsRequest.getStepDelta())
            );
        } catch (FunctionService.FunctionServiceException e) {
            return ResponseEntity.badRequest().build();
        }
    }
}
