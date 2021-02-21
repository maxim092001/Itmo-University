package org.mathoptimization.math_optimization;

import org.mathoptimization.math_optimization.methods.DichotomyMethod;
import org.mathoptimization.math_optimization.methods.GoldenRatio;
import org.mathoptimization.math_optimization.methods.OptimizationMethod;

public class MathOptimizationApplication {

	public static void main(String[] args) {

		OptimizationMethod ratio = new GoldenRatio(
				6.0,
				9.9,
				x -> Math.pow(Math.log10(x - 2), 2.0) + Math.pow(Math.log10(10 - x), 2.0) - Math.pow(x, 0.2),
				1e-9
		);

		OptimizationMethod dichotomyMethod = new DichotomyMethod(
				6.0,
				9.9,
				x -> Math.pow(Math.log10(x - 2), 2.0) + Math.pow(Math.log10(10 - x), 2.0) - Math.pow(x, 0.2),
				1e-9,
				1e-10
		);

		ratio.run();
		ratio.getParameters().forEach(System.out::println);

		dichotomyMethod.run();
		dichotomyMethod.getParameters().forEach(System.out::println);
	}

}
