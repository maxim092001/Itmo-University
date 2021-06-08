package org.mathoptimization.math_optimization.services;

import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.util.DigestUtils;

import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;
import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;

@Service
@NoArgsConstructor
public class FunctionService {
    private static final String SINGLE_LINE_FUNCTION_SOURCE_TEMPLATE;
    private static final Path tempDir;
    private static final Map<String, Function<Double, Double>> generatedFunctions = new ConcurrentHashMap<>();
    private static final ClassLoader generatedFunctionsClassLoader;

    public List<Point> calcPoints(Function<Double, Double> function, double from, double to, double stepDelta)
            throws FunctionServiceException{
        if (from + stepDelta >= to || stepDelta < 0) {
            throw new FunctionServiceException("Too small interval");
        }

        List<Point> result = new ArrayList<>();
        for (double x = from; x <= to; x += stepDelta) {
            result.add(new Point(x, function.apply(x)));
        }
        return result;
    }

    public Function<Double, Double> parse(String source) throws FunctionServiceException {
        if (source == null) {
            throw new FunctionServiceException(new NullPointerException());
        }

        Function<Double, Double> function = generatedFunctions.get(source);
        if (function == null) {
            function = internalParse(source);
            generatedFunctions.putIfAbsent(source, function);
        }
        return function;
    }

    private Function<Double, Double> internalParse(String source) throws FunctionServiceException {
        String simpleGeneratedClassName = "Func" + DigestUtils.md5DigestAsHex(source.getBytes(StandardCharsets.UTF_8));
        Path javaTempSourceFile;
        try {
            Path path = tempDir.resolve(simpleGeneratedClassName + ".java");
            Files.deleteIfExists(path);
            javaTempSourceFile = Files.createFile(path);
        } catch (IOException e) {
            throw new FunctionServiceException("Can't create temp file", e);
        }

        source = String.format(SINGLE_LINE_FUNCTION_SOURCE_TEMPLATE, simpleGeneratedClassName, source);

        try {
            Files.writeString(javaTempSourceFile, source, StandardOpenOption.CREATE, StandardOpenOption.WRITE);
        } catch (IOException e) {
            throw new FunctionServiceException(e);
        }

        JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
        if (compiler == null) {
            throw new FunctionServiceException("Can't find system java compiler");
        }

        try {
            compiler.run(null, null, null, javaTempSourceFile.toString());
        } catch (Exception e) {
            throw new FunctionServiceException("Compiler invocation failed", e);
        }

        try {
            Class<?> generatedClass = Class.forName(simpleGeneratedClassName, true, generatedFunctionsClassLoader);
            Constructor<?> constructor = generatedClass.getConstructor();
            //noinspection unchecked
            return (Function<Double, Double>) constructor.newInstance();
        } catch (ClassNotFoundException | NoSuchMethodException e) {
            throw new FunctionServiceException("Can't find default constructor for the " +
                    "generated class by name [name=" + simpleGeneratedClassName + "].", e);
        } catch (IllegalAccessException | InstantiationException | InvocationTargetException e) {
            throw new FunctionServiceException("Can't instantiate generated function " +
                    "[name=" + simpleGeneratedClassName + "].", e);
        }
    }

    @Data
    public static class Point {
        private final double x, y;
    }

    public static class FunctionServiceException extends Exception {
        public FunctionServiceException(String message) {
            super(message);
        }

        public FunctionServiceException(String message, Throwable cause) {
            super(message, cause);
        }

        public FunctionServiceException(Throwable cause) {
            super(cause);
        }
    }

    static {
        SINGLE_LINE_FUNCTION_SOURCE_TEMPLATE = """
                        import java.util.function.Function;
                        import static java.lang.Math.*;
                        
                        public class %s implements Function<Double, Double> {
                            @Override
                            public Double apply(Double x) {
                                return %s;
                            }
                        }
                """;

        try {
            tempDir = Files.createTempDirectory(FunctionService.class.getSimpleName());
        } catch (IOException e) {
            throw new RuntimeException("Can't create temp directory", e);
        }

        try {
            generatedFunctionsClassLoader = URLClassLoader.newInstance(new URL[]{tempDir.toUri().toURL()});
        } catch (MalformedURLException e) {
            throw new RuntimeException("Unexpectedly malformed url", e);
        }
    }
}
