package org.mathoptimization.math_optimization.dto;

import lombok.Data;
import org.mathoptimization.math_optimization.parameters.Parameters;

import java.util.List;

@Data
public class OptimizationResult {
    private final List<Parameters> steps;
    private final double value, argument;
}
