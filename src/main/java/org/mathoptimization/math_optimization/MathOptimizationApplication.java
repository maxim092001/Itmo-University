package org.mathoptimization.math_optimization;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.security.servlet.SecurityAutoConfiguration;

@SpringBootApplication(exclude = {SecurityAutoConfiguration.class})
public class MathOptimizationApplication {
	public static void main(String[] args) {
		 SpringApplication.run(MathOptimizationApplication.class, args);
	}

}
