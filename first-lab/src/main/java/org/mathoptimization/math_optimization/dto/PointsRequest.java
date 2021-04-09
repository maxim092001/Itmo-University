package org.mathoptimization.math_optimization.dto;

import lombok.Data;

@Data
public class PointsRequest {
    private final String function;
    private final double from, to, stepDelta;
}
