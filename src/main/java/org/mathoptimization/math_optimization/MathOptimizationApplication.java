package org.mathoptimization.math_optimization;

import org.mathoptimization.math_optimization.methods.*;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.security.servlet.SecurityAutoConfiguration;

import java.text.DecimalFormat;
import java.util.function.Function;

//@SpringBootApplication(exclude = {SecurityAutoConfiguration.class})
public class MathOptimizationApplication {

	public static void main(String[] args) {
//		 SpringApplication.run(MathOptimizationApplication.class, args);

//		Function<Double, OptimizationMethod> dichotomyMethodFactory = (eps) -> new DichotomyMethod(
//				6.0,
//				9.9,
//				x -> Math.pow(Math.log10(x - 2), 2.0) + Math.pow(Math.log10(10 - x), 2.0) - Math.pow(x, 0.2),
//				eps,
//				1e-10
//		);
//
//		double[] epss = {1e-1, 5e-2, 1e-2, 5e-3, 1e-3, 5e-4, 1e-4, 5e-5, 1e-5, 5e-6, 1e-6, 5e-7, 1e-7, 5e-8, 1e-8, 5e-9, 1e-9};
//		for (double eps : epss) {
//			OptimizationMethod method = dichotomyMethodFactory.apply(eps);
//			method.calculate();
//
//			System.out.printf("%.10f", eps);
//			System.out.print(", ");
//			System.out.print(method.getParameters().size() * 2);
//			System.out.print(", ");
//			System.out.print(method.getMinimumArgument());
//			System.out.print(", ");
//			System.out.println(method.getMinimumValue());
//		}

//
//		Function<Double, OptimizationMethod> fibonacciMethodFactory = (eps) -> new FibonacciMethod(
//				6.0,
//				9.9,
//				x -> Math.pow(Math.log10(x - 2), 2.0) + Math.pow(Math.log10(10 - x), 2.0) - Math.pow(x, 0.2),
//				eps
//		);
//
//		double[] epss = {1e-1, 5e-2, 1e-2, 5e-3, 1e-3, 5e-4, 1e-4, 5e-5, 1e-5, 5e-6, 1e-6, 5e-7, 1e-7, 5e-8, 1e-8, 5e-9, 1e-9};
//		for (double eps : epss) {
//			OptimizationMethod method = fibonacciMethodFactory.apply(eps);
//			method.calculate();
//
//			System.out.printf("%.10f", eps);
//			System.out.print(", ");
//			System.out.print(method.getParameters().size() * 2 + 1);
//			System.out.print(", ");
//			System.out.print(method.getMinimumArgument());
//			System.out.print(", ");
//			System.out.println(method.getMinimumValue());
//		}
//
//		Function<Double, OptimizationMethod> parabolaMethodFactory = (eps) -> new FibonacciMethod(
//				6.0,
//				9.9,
//				x -> Math.pow(Math.log10(x - 2), 2.0) + Math.pow(Math.log10(10 - x), 2.0) - Math.pow(x, 0.2),
//				eps
//		);
//
//		double[] epss = {1e-1, 5e-2, 1e-2, 5e-3, 1e-3, 5e-4, 1e-4, 5e-5, 1e-5, 5e-6, 1e-6, 5e-7, 1e-7, 5e-8, 1e-8, 5e-9, 1e-9};
//		for (double eps : epss) {
//			OptimizationMethod method = parabolaMethodFactory.apply(eps);
//			method.calculate();
//
//			System.out.printf("%.10f", eps);
//			System.out.print(", ");
//			System.out.print(method.getParameters().size() * 3 + 1);
//			System.out.print(", ");
//			System.out.print(method.getMinimumArgument());
//			System.out.print(", ");
//			System.out.println(method.getMinimumValue());
//		}


//		Function<Double, OptimizationMethod> goldenRatio = (eps) -> new GoldenRatioMethod(
//				6.0,
//				9.9,
//				x -> Math.pow(Math.log10(x - 2), 2.0) + Math.pow(Math.log10(10 - x), 2.0) - Math.pow(x, 0.2),
//				eps
//		);
//
//		double[] epss = {1e-1, 5e-2, 1e-2, 5e-3, 1e-3, 5e-4, 1e-4, 5e-5, 1e-5, 5e-6, 1e-6, 5e-7, 1e-7, 5e-8, 1e-8, 5e-9, 1e-9};
//		for (double eps : epss) {
//			OptimizationMethod method = goldenRatio.apply(eps);
//			method.calculate();
//
//			System.out.printf("%.10f", eps);
//			System.out.print(", ");
//			System.out.print(method.getParameters().size() * 2 + 1);
//			System.out.print(", ");
//			System.out.print(method.getMinimumArgument());
//			System.out.print(", ");
//			System.out.println(method.getMinimumValue());
//		}


		Function<Double, OptimizationMethod> brents = (eps) -> new BrentsMethod(
				6.0,
				9.9,
				x -> Math.pow(Math.log10(x - 2), 2.0) + Math.pow(Math.log10(10 - x), 2.0) - Math.pow(x, 0.2),
				eps
		);

		double[] epss = {1e-1, 5e-2, 1e-2, 5e-3, 1e-3, 5e-4, 1e-4, 5e-5, 1e-5, 5e-6, 1e-6, 5e-7, 1e-7, 5e-8, 1e-8, 5e-9, 1e-9};
		for (double eps : epss) {
			OptimizationMethod method = brents.apply(eps);
			method.calculate();

			System.out.printf("%.10f", eps);
			System.out.print(", ");
			System.out.print(method.getParameters().size() + 2);
			System.out.print(", ");
			System.out.print(method.getMinimumArgument());
			System.out.print(", ");
			System.out.println(method.getMinimumValue());
		}



//		OptimizationMethod ratioMethod = new GoldenRatioMethod(
//				6.0,
//				9.9,
//				x -> Math.pow(Math.log10(x - 2), 2.0) + Math.pow(Math.log10(10 - x), 2.0) - Math.pow(x, 0.2),
//				1e-9
//		);
//
//		OptimizationMethod dichotomyMethod = new DichotomyMethod(
//				6.0,
//				9.9,
//				x -> Math.pow(Math.log10(x - 2), 2.0) + Math.pow(Math.log10(10 - x), 2.0) - Math.pow(x, 0.2),
//				1e-9,
//				1e-10
//		);
//
//		OptimizationMethod fibonacciMethod = new FibonacciMethod(
//				6.0,
//				9.9,
//				x -> Math.pow(Math.log10(x - 2), 2.0) + Math.pow(Math.log10(10 - x), 2.0) - Math.pow(x, 0.2),
//				1e-9
//		);
//
//
//		OptimizationMethod parabolaMethod = new ParabolaMethod(
//				6.0,
//				9.9,
//				(x) -> {
//					double lg1 = Math.log10(x - 2.0);
//					double lg2 = Math.log10(10.0 - x);
//					return lg1 * lg1 + lg2 * lg2 - Math.pow(x, 0.2);
//				},
//				1e-6
//		);
//
//		OptimizationMethod brentsMethod = new BrentsMethod(
//				6.0,
//				9.9,
//				(x) -> {
//					double lg1 = Math.log10(x - 2.0);
//					double lg2 = Math.log10(10.0 - x);
//					return lg1 * lg1 + lg2 * lg2 - Math.pow(x, 0.2);
//				},
//				1e-9
//		);
//
//		ratioMethod.calculate();
//		System.out.println("Ratio: " + ratioMethod.getMinimumArgument() + " " + ratioMethod.getMinimumValue());
//
//		dichotomyMethod.calculate();
//		System.out.println("Dichotomy: " + dichotomyMethod.getMinimumArgument() + " " + dichotomyMethod.getMinimumValue());
//
//		fibonacciMethod.calculate();
//		System.out.println("Fibonacci: " + fibonacciMethod.getMinimumArgument() + " " + fibonacciMethod.getMinimumValue());
//
//		parabolaMethod.calculate();
//		System.out.println("Parabola: " + parabolaMethod.getMinimumArgument() + " " + parabolaMethod.getMinimumValue());
//
//		brentsMethod.calculate();
//		System.out.println("Brent's Method: " + brentsMethod.getMinimumArgument() + " " + brentsMethod.getMinimumValue());
	}

}
