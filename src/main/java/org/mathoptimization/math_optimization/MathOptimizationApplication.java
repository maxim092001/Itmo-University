package org.mathoptimization.math_optimization;

import org.mathoptimization.math_optimization.methods.*;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.security.servlet.SecurityAutoConfiguration;

import java.text.DecimalFormat;

//@SpringBootApplication(exclude = {SecurityAutoConfiguration.class})
public class MathOptimizationApplication {

	public static void main(String[] args) {
//		 SpringApplication.run(MathOptimizationApplication.class, args);

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
		OptimizationMethod brentsMethod = new BrentsMethod(
				6.0,
				9.9,
				(x) -> {
					double lg1 = Math.log10(x - 2.0);
					double lg2 = Math.log10(10.0 - x);
					return lg1 * lg1 + lg2 * lg2 - Math.pow(x, 0.2);
				},
				1e-9
		);
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
		brentsMethod.calculate();
//		System.out.println("Brent's Method: " + brentsMethod.getMinimumArgument() + " " + brentsMethod.getMinimumValue());
		brentsMethod.calculate();
		double xLeft = 1.0;
		double xRight = 1.0;
		var parameters = brentsMethod.getParameters();
		DecimalFormat df = new DecimalFormat("#.##########");
		for (int i = 0; i < parameters.size(); i++) {
			String x = parameters.get(i).toString();
//            System.out.println(x);
			x = x.replace("BrentsParameters", "");
			x = x.replace("AbstractParameters", "");
			x = x.replace("(", "");
			x = x.replace(")", "");
			x = x.replace("super=", "");
			x = x.replace("left=", "");
			x = x.replace("right=", "");
			x = x.replace("value=", "");
			x = x.replace("secondValue=", "");
			x = x.replace("argument=", "");
			x = x.replace("secondArgument=", "");
			x = x.replace("minArgument=", "");
			x = x.replace("thirdValue=", "");
			x = x.replace("thirdArgument=", "");
			x = x.replace("minValue=", "");
			x = x.replace("firstMinimum=", "");
			x = x.replace("secondMinimum=", "");
			x = x.replace("previousSecondMinimum=", "");
			x = x.replace("firstMinimumValue=", "");
			x = x.replace("secondMinimumValue=", "");
			x = x.replace("previousSecondMinimumValue=", "");
			x = x.replace("a0=", "");
			x = x.replace("a1=", "");
			x = x.replace("a2=", "");
			x = x.replace(",", "");
			String[] splitted = x.split(" ");
			double left = Double.parseDouble(splitted[0]);
			double right = Double.parseDouble(splitted[1]);
			double value1 = Double.parseDouble(splitted[2]);
			double argument1 = Double.parseDouble(splitted[3]);
			Double a0 = splitted[4].equals("null") ? null : Double.parseDouble(splitted[4]);
			Double a1 = splitted[5].equals("null") ? null : Double.parseDouble(splitted[5]);
			Double a2 = splitted[6].equals("null") ? null : Double.parseDouble(splitted[6]);
			double firstMinimum = Double.parseDouble(splitted[7]);
			double secondMinimum = Double.parseDouble(splitted[8]);
			double previousSecondMinimum = Double.parseDouble(splitted[9]);
			double firstMinimumValue = Double.parseDouble(splitted[10]);
			double secondMinimumValue = Double.parseDouble(splitted[11]);
			double previousSecondMinimumValue = Double.parseDouble(splitted[12]);
			double ratio = (right - left) / (xRight - xLeft);
			xRight = right;
			xLeft = left;
			System.out.printf("%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s%n",
					(i == 0 ? "" : df.format(ratio)),
					df.format(left),
					df.format(right),
					df.format(argument1),
					df.format(value1),
					(a0 == null ? "-" : df.format(a0)),
					(a1 == null ? "-" : df.format(a1)),
					(a2 == null ? "-" : df.format(a2)),
					df.format(firstMinimum),
					df.format(secondMinimum),
					df.format(previousSecondMinimum),
					df.format(firstMinimumValue),
					df.format(secondMinimumValue),
					df.format(previousSecondMinimumValue)
			);
		}
	}

}
