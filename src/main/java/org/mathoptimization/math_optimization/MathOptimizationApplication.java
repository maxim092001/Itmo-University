package org.mathoptimization.math_optimization;

import org.mathoptimization.math_optimization.methods.*;

public class MathOptimizationApplication {

	public static void main(String[] args) {

		OptimizationMethod ratioMethod = new GoldenRatioMethod(
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

		OptimizationMethod fibonacciMethod = new FibonacciMethod(
				6.0,
				9.9,
				x -> Math.pow(Math.log10(x - 2), 2.0) + Math.pow(Math.log10(10 - x), 2.0) - Math.pow(x, 0.2),
				1e-9,
				20
		);


		OptimizationMethod parabolaMethod = new ParabolaMethod(
				6.0,
				9.9,
				(x) -> {
					double lg1 = Math.log10(x - 2.0);
					double lg2 = Math.log10(10.0 - x);
					return lg1 * lg1 + lg2 * lg2 - Math.pow(x, 0.2);
				},
				1e-6
		);

		ratioMethod.calculate();
		System.out.println("Ratio: " + ratioMethod.getMinimumArgument() + " " + ratioMethod.getMinimumValue());

		dichotomyMethod.calculate();
		System.out.println("Dichotomy: " + dichotomyMethod.getMinimumArgument() + " " + dichotomyMethod.getMinimumValue());

		fibonacciMethod.calculate();
		System.out.println("Fibonacci: " + fibonacciMethod.getMinimumArgument() + " " + fibonacciMethod.getMinimumValue());

		parabolaMethod.calculate();
		System.out.println("Parabola: " + parabolaMethod.getMinimumArgument() + " " + parabolaMethod.getMinimumValue());
	}

}
