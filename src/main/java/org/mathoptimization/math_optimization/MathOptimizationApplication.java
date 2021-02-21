package org.mathoptimization.math_optimization;

import org.mathoptimization.math_optimization.methods.GoldenRatio;
import org.mathoptimization.math_optimization.methods.MethodStep;
import org.mathoptimization.math_optimization.methods.OptimizationMethod;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

public class MathOptimizationApplication {

	public static void main(String[] args) {

		OptimizationMethod ratio = new GoldenRatio(
				6.0,
				9.9,
				x -> Math.pow(Math.log10(x - 2), 2.0) + Math.pow(Math.log10(10 - x), 2.0) - Math.pow(x, 0.2),
				1e-9
		);

		ratio.run();
		ratio.getSteps().forEach(System.out::println);
		System.out.format("%s %s\n", ratio.getMinimumArgument(), ratio.getMinimumValue());
	}

}
